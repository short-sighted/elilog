;;; elilog-async.el --- Asynchronous logging processing module  -*- lexical-binding: t; -*-

;; Author: Elilog Development Team
;; Maintainer: Elilog Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (async "1.9"))
;; Keywords: logging, async, performance, emacs-async
;; URL: https://github.com/elilog/elilog

;;; Commentary:
;; This module provides emacs-async based asynchronous logging processing functionality
;; including async file writing, batch processing, network sending and other high-performance operations.
;;
;; 这个模块提供基于emacs-async的异步日志处理功能，
;; 包括异步文件写入、批量处理、网络发送等高性能操作。
;;
;; Key Features / 主要特性：
;; - True asynchronous processing with subprocess isolation / 带子进程隔离的真正异步处理
;; - Batch processing for high-throughput scenarios / 用于高吞吐量场景的批量处理
;; - Smart async strategy based on message complexity / 基于消息复杂度的智能异步策略
;; - Process pool management for resource control / 用于资源控制的进程池管理
;; - Automatic fallback to sync when async unavailable / 异步不可用时自动降级为同步
;; - Performance monitoring and statistics / 性能监控和统计
;;
;; Usage Examples / 使用示例：
;; (elilog-async-create-file-sink "/var/log/async.log")
;; (elilog-async-create-batch-file-sink "/var/log/batch.log")
;; (elilog-async-statistics)

;;; Code:

(require 'cl-lib)
(require 'elilog-formatters)
(require 'elilog-sinks)

;; Optional dependency on emacs-async / 可选依赖emacs-async
(defvar elilog-async-available-p
  (condition-case nil
      (progn (require 'async) t)
    (error nil))
  "Check if emacs-async package is available.
检查emacs-async包是否可用。")

;;; ===== Configuration Variables / 配置变量 =====

(defgroup elilog-async nil
  "Elilog asynchronous processing configuration group.
Elilog异步处理配置组。"
  :group 'elilog
  :prefix "elilog-async-")

(defcustom elilog-async-enabled t
  "Whether to enable asynchronous log processing.
是否启用异步日志处理。

If emacs-async is not available, will automatically fallback to synchronous processing.
如果emacs-async不可用，将自动降级为同步处理。"
  :type 'boolean
  :group 'elilog-async)

(defcustom elilog-async-batch-size 50
  "Batch size for asynchronous batch writing.
异步批量写入的批次大小。

Higher values improve throughput but increase memory usage.
更高的值可以提高吞吐量但会增加内存使用。"
  :type 'integer
  :group 'elilog-async)

(defcustom elilog-async-batch-timeout 2.0
  "Timeout for batch writing in seconds.
批量写入的超时时间（秒）。

Batches will be flushed automatically after this timeout.
批次会在此超时后自动刷新。"
  :type 'float
  :group 'elilog-async)

(defcustom elilog-async-process-pool-size 3
  "Size of asynchronous process pool, limits concurrent process count.
异步进程池大小，限制并发进程数。

Higher values allow more concurrency but consume more system resources.
更高的值允许更多并发但消耗更多系统资源。"
  :type 'integer
  :group 'elilog-async)

(defcustom elilog-async-retry-attempts 3
  "Number of retry attempts when asynchronous operations fail.
异步操作失败时的重试次数。

Failed operations will be retried up to this many times before giving up.
失败的操作在放弃之前将重试最多这么多次。"
  :type 'integer
  :group 'elilog-async)

(defcustom elilog-async-message-threshold 100
  "Message length threshold for enabling asynchronous processing.
启用异步处理的消息长度阈值。

Logs shorter than this will use synchronous processing to reduce overhead.
短于此长度的日志将使用同步处理以减少开销。"
  :type 'integer
  :group 'elilog-async)

;;; ===== Internal State Variables / 内部状态变量 =====

(defvar elilog-async--active-processes 0
  "Number of currently active asynchronous processes.
当前活跃的异步进程数。")

(defvar elilog-async--batch-queues (make-hash-table :test 'equal)
  "Batch writing queues, grouped by file path.
批量写入队列，按文件路径分组。

Each entry contains a list of pending log events for that file.
每个条目包含该文件的待处理日志事件列表。")

(defvar elilog-async--batch-timers (make-hash-table :test 'equal)
  "Batch writing timers for automatic flushing.
用于自动刷新的批量写入定时器。

Each timer automatically flushes its corresponding batch queue when triggered.
每个定时器在触发时自动刷新其对应的批量队列。")

(defvar elilog-async--statistics (list :total-async 0 :total-sync 0 :errors 0)
  "Statistics for asynchronous processing operations.
异步处理操作的统计信息。

:total-async - Total number of messages processed asynchronously
:total-sync - Total number of messages processed synchronously as fallback
:errors - Number of async processing errors

:total-async - 异步处理的消息总数
:total-sync - 作为降级处理的同步消息总数
:errors - 异步处理错误数量")

;;; ===== Core Asynchronous Functions / 核心异步函数 =====

(defun elilog-async--available-p ()
  "Check if asynchronous functionality is available.
检查异步功能是否可用。

Returns t if both async is enabled and emacs-async package is available.
如果异步已启用且emacs-async包可用，则返回t。"
  (and elilog-async-enabled elilog-async-available-p))

(defun elilog-async--should-use-async-p (event)
  "Determine if asynchronous processing should be used for this event.
判断是否应该为此事件使用异步处理。

EVENT: The log event to evaluate / 要评估的日志事件

Returns t if async processing is recommended based on:
根据以下条件返回t表示推荐异步处理：
- Async is available / 异步功能可用
- Process pool has capacity / 进程池有容量
- Message meets complexity threshold / 消息满足复杂度阈值

Criteria for async processing:
异步处理的标准：
- Message length >= threshold / 消息长度 >= 阈值
- Event has properties / 事件有属性
- Event level is error or fatal / 事件级别是错误或致命"
  (and (elilog-async--available-p)
       (< elilog-async--active-processes elilog-async-process-pool-size)
       (or (>= (length (elilog-event-message event)) elilog-async-message-threshold)
           (elilog-event-properties event)
           (memq (elilog-event-level event) '(error fatal)))))

(defun elilog-async--with-process-limit (operation callback)
  "Execute asynchronous operation within process pool limits.
在进程池限制下执行异步操作。

OPERATION: Function to execute asynchronously / 要异步执行的函数
CALLBACK: Function to call when operation completes / 操作完成时调用的函数

This function manages the process pool to prevent resource exhaustion.
此函数管理进程池以防止资源耗尽。"
  (if (< elilog-async--active-processes elilog-async-process-pool-size)
      (progn
        (cl-incf elilog-async--active-processes)
        (plist-put elilog-async--statistics :total-async 
                   (1+ (plist-get elilog-async--statistics :total-async)))
        (async-start
         operation
         (lambda (result)
           (cl-decf elilog-async--active-processes)
           (when (and result (eq (car-safe result) 'error))
             (plist-put elilog-async--statistics :errors 
                        (1+ (plist-get elilog-async--statistics :errors)))
             (message "异步日志操作失败: %s" (cdr result)))
           (when callback (funcall callback result)))))
    ;; 进程池满，降级为同步执行
    (plist-put elilog-async--statistics :total-sync 
               (1+ (plist-get elilog-async--statistics :total-sync)))
    (condition-case err
        (funcall operation)
      (error (message "同步日志操作失败: %s" (error-message-string err))))))

(defun elilog-async--with-retry (operation max-retries)
  "带重试机制的异步操作。"
  `(lambda ()
     (let ((retries 0)
           (result nil))
       (while (and (< retries ,max-retries) (not result))
         (condition-case err
             (setq result (funcall ,operation))
           (error
            (setq retries (1+ retries))
            (when (< retries ,max-retries)
              (sleep-for (expt 2 retries))))))  ; 指数退避
       (or result (list 'error "最大重试次数已达到")))))

;;; ===== 异步文件写入 =====

(defun elilog-async--write-to-file (formatted file-path &optional encoding)
  "异步写入单个格式化的日志到文件。"
  (if (not (elilog-async--available-p))
      ;; 降级为同步写入
      (progn
        (let ((coding-system-for-write (or encoding 'utf-8)))
          (with-temp-buffer
            (insert formatted "\n")
            (append-to-file (point-min) (point-max) file-path)))
        'sync-success)
    ;; 异步写入
    (elilog-async--with-process-limit
     (elilog-async--with-retry
      `(lambda ()
         (let ((coding-system-for-write ',(or encoding 'utf-8)))
           (with-temp-buffer
             (insert ,formatted "\n")
             (append-to-file (point-min) (point-max) ,file-path))
           'async-success))
      elilog-async-retry-attempts)
     nil)))

(defun elilog-async--batch-write-to-file (events file-path formatter &optional encoding)
  "批量异步写入多个事件到文件。"
  (when events
    (if (not (elilog-async--available-p))
        ;; 降级为同步批量写入
        (let ((coding-system-for-write (or encoding 'utf-8))
              (content ""))
          (dolist (event events)
            (let ((formatted (elilog-formatters-format-event event formatter)))
              (when formatted
                (setq content (concat content formatted "\n")))))
          (when (> (length content) 0)
            (with-temp-buffer
              (insert content)
              (append-to-file (point-min) (point-max) file-path))))
      ;; 异步批量写入
      (elilog-async--with-process-limit
       (elilog-async--with-retry
        `(lambda ()
           (let ((coding-system-for-write ',(or encoding 'utf-8))
                 (content ""))
             ;; 在子进程中格式化所有事件
             (dolist (event ',events)
               (let ((formatted (funcall ',(elilog-formatter-format-fn formatter) 
                                        event ',formatter)))
                 (when formatted
                   (setq content (concat content formatted "\n")))))
             ;; 一次性写入所有内容
             (when (> (length content) 0)
               (with-temp-buffer
                 (insert content)
                 (append-to-file (point-min) (point-max) ,file-path)))
             (list 'batch-success (length ',events))))
        elilog-async-retry-attempts)
       (lambda (result)
         (when (and result (eq (car result) 'batch-success))
           (message "成功批量写入 %d 条日志到 %s" (cadr result) file-path)))))))

;;; ===== 批量队列管理 =====

(defun elilog-async--add-to-batch (event file-path formatter)
  "将事件添加到批量写入队列。"
  (let ((queue-key file-path))
    ;; 添加事件到队列
    (push event (gethash queue-key elilog-async--batch-queues))
    
    ;; 设置或重置定时器
    (when-let* ((old-timer (gethash queue-key elilog-async--batch-timers)))
      (cancel-timer old-timer))
    
    (puthash queue-key
             (run-with-timer elilog-async-batch-timeout nil
                           'elilog-async--flush-batch queue-key formatter)
             elilog-async--batch-timers)
    
    ;; 检查是否达到批次大小
    (when (>= (length (gethash queue-key elilog-async--batch-queues))
              elilog-async-batch-size)
      (elilog-async--flush-batch queue-key formatter))))

(defun elilog-async--flush-batch (queue-key formatter)
  "刷新指定队列的批量写入。"
  (when-let* ((events (gethash queue-key elilog-async--batch-queues)))
    (remhash queue-key elilog-async--batch-queues)
    (when-let* ((timer (gethash queue-key elilog-async--batch-timers)))
      (cancel-timer timer)
      (remhash queue-key elilog-async--batch-timers))
    
    (elilog-async--batch-write-to-file (reverse events) queue-key formatter)))

(defun elilog-async-flush-all-batches ()
  "刷新所有批量写入队列。"
  (interactive)
  (maphash (lambda (queue-key _events)
             (elilog-async--flush-batch queue-key nil))
           elilog-async--batch-queues))

;;; ===== 异步网络操作 =====

(defun elilog-async--send-to-remote (formatted url headers)
  "异步发送日志到远程服务器。"
  (if (not (elilog-async--available-p))
      ;; 降级为同步发送
      (condition-case err
          (let ((url-request-method "POST")
                (url-request-extra-headers headers)
                (url-request-data formatted))
            (url-retrieve-synchronously url t nil 10)
            'sync-sent)
        (error (list 'error (error-message-string err))))
    ;; 异步发送
    (elilog-async--with-process-limit
     (elilog-async--with-retry
      `(lambda ()
         (condition-case err
             (progn
               (require 'url)
               (let ((url-request-method "POST")
                     (url-request-extra-headers ',headers)
                     (url-request-data ,formatted))
                 (with-current-buffer (url-retrieve-synchronously ,url t nil 30)
                   (goto-char (point-min))
                   (if (re-search-forward "^HTTP/[0-9.]+ 2[0-9][0-9]" nil t)
                       'async-sent
                     (list 'error "HTTP错误")))))
           (error (list 'error (error-message-string err)))))
      elilog-async-retry-attempts)
     nil)))

;;; ===== 异步Sink实现 =====

(defun elilog-async--sink-write-to-file (event sink)
  "异步文件Sink写入函数。"
  (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink)))
        (file-path (plist-get (elilog-sink-config sink) :file-path))
        (encoding (plist-get (elilog-sink-config sink) :encoding)))
    (when formatted
      (if (elilog-async--should-use-async-p event)
          (elilog-async--write-to-file formatted file-path encoding)
        ;; 短消息使用同步写入
        (let ((coding-system-for-write (or encoding 'utf-8)))
          (with-temp-buffer
            (insert formatted "\n")
            (append-to-file (point-min) (point-max) file-path)))))))

(defun elilog-async--sink-batch-write-to-file (event sink)
  "批量异步文件Sink写入函数。"
  (let ((file-path (plist-get (elilog-sink-config sink) :file-path))
        (formatter (elilog-sink-formatter sink)))
    (elilog-async--add-to-batch event file-path formatter)))

(defun elilog-async--sink-write-to-remote (event sink)
  "异步远程Sink写入函数。"
  (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink)))
        (url (plist-get (elilog-sink-config sink) :url))
        (headers (plist-get (elilog-sink-config sink) :headers)))
    (when formatted
      (elilog-async--send-to-remote formatted url headers))))

;;; ===== Public API - Async Sink Creation Functions / 公共API - 异步Sink创建函数 =====

(defun elilog-async-create-file-sink (file-path &optional formatter encoding)
  "Create an asynchronous file sink for high-performance logging.
创建用于高性能日志记录的异步文件Sink。

FILE-PATH: Target file path / 目标文件路径
FORMATTER: Optional formatter, defaults to text formatter / 可选的格式化器，默认使用文本格式化器
ENCODING: File encoding, defaults to utf-8 / 文件编码，默认utf-8

Returns an elilog-sink configured for asynchronous file writing.
返回配置为异步文件写入的elilog-sink。"
  (make-elilog-sink
   :name "async-file"
   :type 'async-file
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:file-path ,file-path :encoding ,(or encoding 'utf-8))
   :write-fn 'elilog-async--sink-write-to-file))

(defun elilog-async-create-batch-file-sink (file-path &optional formatter encoding)
  "Create a batch asynchronous file sink for high-throughput scenarios.
创建用于高吞吐量场景的批量异步文件Sink。

FILE-PATH: Target file path / 目标文件路径
FORMATTER: Optional formatter / 可选的格式化器
ENCODING: File encoding / 文件编码

Suitable for high-frequency logging, automatically batches writes for performance.
适合高频日志写入，会自动批量处理以提高性能。"
  (make-elilog-sink
   :name "async-batch-file"
   :type 'async-batch-file
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:file-path ,file-path :encoding ,(or encoding 'utf-8))
   :write-fn 'elilog-async--sink-batch-write-to-file))

(defun elilog-async-create-remote-sink (url &optional formatter headers)
  "创建异步远程Sink。
URL: 目标URL
FORMATTER: 可选的格式化器，建议使用JSON格式化器
HEADERS: HTTP头部"
  (make-elilog-sink
   :name "async-remote"
   :type 'async-remote
   :formatter (or formatter (elilog-formatters-create-json))
   :config `(:url ,url :headers ,headers)
   :write-fn 'elilog-async--sink-write-to-remote))

(defun elilog-async-create-smart-file-sink (file-path &optional formatter encoding)
  "创建智能异步文件Sink。
根据消息复杂度自动选择同步或异步处理。"
  (make-elilog-sink
   :name "smart-async-file"
   :type 'smart-async-file
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:file-path ,file-path :encoding ,(or encoding 'utf-8))
   :write-fn 'elilog-async--sink-write-to-file))

;;; ===== 性能监控和统计 =====

(defun elilog-async-statistics ()
  "获取异步处理统计信息。"
  (interactive)
  (let ((stats elilog-async--statistics))
    (message "异步日志统计: 异步=%d, 同步=%d, 错误=%d, 活跃进程=%d"
             (plist-get stats :total-async)
             (plist-get stats :total-sync)
             (plist-get stats :errors)
             elilog-async--active-processes)
    stats))

(defun elilog-async-reset-statistics ()
  "重置异步处理统计信息。"
  (interactive)
  (setq elilog-async--statistics (list :total-async 0 :total-sync 0 :errors 0)))

(defun elilog-async-status ()
  "显示异步模块状态。"
  (interactive)
  (message "异步模块状态: %s, emacs-async: %s, 活跃进程: %d/%d"
           (if elilog-async-enabled "启用" "禁用")
           (if elilog-async-available-p "可用" "不可用")
           elilog-async--active-processes
           elilog-async-process-pool-size))

;;; ===== 清理和关闭 =====

(defun elilog-async-shutdown ()
  "关闭异步模块并清理资源。"
  (interactive)
  ;; 刷新所有批量队列
  (elilog-async-flush-all-batches)
  
  ;; 取消所有定时器
  (maphash (lambda (_key timer)
             (cancel-timer timer))
           elilog-async--batch-timers)
  (clrhash elilog-async--batch-timers)
  (clrhash elilog-async--batch-queues)
  
  ;; 等待异步进程完成
  (let ((timeout 10)
        (start-time (current-time)))
    (while (and (> elilog-async--active-processes 0)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sleep-for 0.1)))
  
  (when (> elilog-async--active-processes 0)
    (message "警告: 仍有 %d 个异步进程未完成" elilog-async--active-processes))
  
  (message "异步模块已关闭"))

;; 在Emacs退出时自动清理
(add-hook 'kill-emacs-hook 'elilog-async-shutdown)

;;; ===== 兼容性函数 =====

(defun elilog-async-enable ()
  "启用异步处理。"
  (interactive)
  (setq elilog-async-enabled t)
  (message "异步处理已启用 (emacs-async: %s)" 
           (if elilog-async-available-p "可用" "不可用")))

(defun elilog-async-disable ()
  "禁用异步处理。"
  (interactive)
  (setq elilog-async-enabled nil)
  (elilog-async-flush-all-batches)
  (message "异步处理已禁用"))

(provide 'elilog-async)

;;; elilog-async.el ends here
