;;; elilog-sinks.el --- Log output targets module  -*- lexical-binding: t; -*-

;; Author: Elilog Development Team
;; Maintainer: Elilog Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: logging, sinks, output, targets
;; URL: https://github.com/elilog/elilog

;;; Commentary:
;; This module contains all sink implementations for the Elilog logging framework.
;; Supports console, file, buffer, remote output and many other targets.
;;
;; 这个模块包含了Elilog日志框架的所有Sink实现，
;; 支持控制台、文件、缓冲区、远程输出等多种目标。
;;
;; Key Features / 主要特性：
;; - Console output for immediate feedback / 用于即时反馈的控制台输出
;; - File output with encoding support / 支持编码的文件输出
;; - Buffer output for in-Emacs logging / 用于Emacs内部日志的缓冲区输出
;; - Rolling file output with size/time limits / 带大小/时间限制的滚动文件输出
;; - Remote output via HTTP/HTTPS / 通过HTTP/HTTPS的远程输出
;; - Async sinks for high performance / 用于高性能的异步输出目标
;; - Composite sinks for multiple outputs / 用于多重输出的组合输出目标
;;
;; Usage Examples / 使用示例：
;; (elilog-sinks-create-console)
;; (elilog-sinks-create-file "/var/log/app.log")
;; (elilog-sinks-create-async-file "/var/log/async.log")

;;; Code:

(require 'cl-lib)
(require 'elilog-formatters)

;; Optional async module loading / 可选加载异步模块
(defvar elilog-sinks--async-available-p
  (condition-case nil
      (progn (require 'elilog-async) t)
    (error nil))
  "Check if async module is available.
检查异步模块是否可用。")

;;; ===== Sink结构定义 =====

(cl-defstruct elilog-sink
  "Log output target structure for directing log output.
日志输出目标结构体，用于指导日志输出。"
  name                  ; Sink name / Sink名称
  type                  ; Sink type (console, file, buffer, remote, etc.) / Sink类型
  formatter             ; Log formatter / 日志格式化器
  filter                ; Log filter / 日志过滤器
  config                ; Configuration data / 配置数据
  write-fn              ; Write function / 写入函数
  dispose-fn            ; Cleanup function / 清理函数
  state)                ; Internal state / 内部状态

;;; ===== 控制台Sink =====

(defun elilog-sinks--write-to-console (event sink)
  "Write log event to console (Emacs message area).
写入日志事件到控制台（Emacs消息区）。

EVENT: The log event to write / 要写入的日志事件
SINK: The console sink instance / 控制台输出目标实例"
  (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink))))
    (when formatted
      (message "%s" formatted))))

(defun elilog-sinks-create-console (&optional formatter)
  "Create a console sink for immediate output.
创建用于即时输出的控制台Sink。

FORMATTER: Optional formatter, defaults to text formatter
          可选的格式化器，默认使用文本格式化器

Returns an elilog-sink structure for console output.
返回用于控制台输出的 elilog-sink 结构体。"
  (make-elilog-sink
   :name "console"
   :type 'console
   :formatter (or formatter (elilog-formatters-create-text))
   :write-fn 'elilog-sinks--write-to-console))

;;; ===== 文件Sink =====

(defun elilog-sinks--write-to-file (event sink)
  "写入到文件。"
  (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink)))
        (file-path (plist-get (elilog-sink-config sink) :file-path))
        (encoding (plist-get (elilog-sink-config sink) :encoding)))
    (when formatted
      (condition-case err
          (let ((coding-system-for-write (or encoding 'utf-8)))
            (with-temp-buffer
              (insert formatted "\n")
              (append-to-file (point-min) (point-max) file-path)))
        (error
         (message "Failed to write to file %s: %s" file-path err))))))

(defun elilog-sinks--dispose-file (sink)
  "清理文件Sink资源。"
  (let ((file-path (plist-get (elilog-sink-config sink) :file-path)))
    (message "File sink disposed: %s" file-path)))

(defun elilog-sinks-create-file (file-path &optional formatter encoding)
  "创建文件Sink。
FILE-PATH: 文件路径
FORMATTER: 可选的格式化器，默认使用文本格式化器
ENCODING: 文件编码，默认utf-8"
  (make-elilog-sink
   :name "file"
   :type 'file
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:file-path ,file-path :encoding ,(or encoding 'utf-8))
   :write-fn 'elilog-sinks--write-to-file
   :dispose-fn 'elilog-sinks--dispose-file))

;;; ===== 缓冲区Sink =====

(defun elilog-sinks--write-to-buffer (event sink)
  "写入到Emacs缓冲区。"
  (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink)))
        (buffer-name (plist-get (elilog-sink-config sink) :buffer-name))
        (max-lines (plist-get (elilog-sink-config sink) :max-lines)))
    (when formatted
      (with-current-buffer (get-buffer-create buffer-name)
        (goto-char (point-max))
        (insert formatted "\n")
        ;; 限制缓冲区行数
        (when max-lines
          (let ((lines (count-lines (point-min) (point-max))))
            (when (> lines max-lines)
              (goto-char (point-min))
              (forward-line (- lines max-lines))
              (delete-region (point-min) (point)))))))))

(defun elilog-sinks--dispose-buffer (sink)
  "清理缓冲区Sink资源。"
  (let ((buffer-name (plist-get (elilog-sink-config sink) :buffer-name)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

(defun elilog-sinks-create-buffer (buffer-name &optional formatter max-lines)
  "创建缓冲区Sink。
BUFFER-NAME: 缓冲区名称
FORMATTER: 可选的格式化器，默认使用文本格式化器
MAX-LINES: 最大行数，超过时删除旧行"
  (make-elilog-sink
   :name "buffer"
   :type 'buffer
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:buffer-name ,buffer-name :max-lines ,max-lines)
   :write-fn 'elilog-sinks--write-to-buffer
   :dispose-fn 'elilog-sinks--dispose-buffer))

;;; ===== 循环文件Sink =====

(defun elilog-sinks--write-to-rolling-file (event sink)
  "写入到循环文件。"
  (let* ((config (elilog-sink-config sink))
         (base-path (plist-get config :base-path))
         (max-size (plist-get config :max-size))
         (max-files (plist-get config :max-files))
         (current-file (or (plist-get (elilog-sink-state sink) :current-file)
                          (concat base-path ".0"))))
    
    ;; 检查文件大小
    (when (and (file-exists-p current-file)
               max-size
               (> (file-attribute-size (file-attributes current-file)) max-size))
      ;; 滚动文件
      (elilog-sinks--roll-files base-path max-files)
      (setq current-file (concat base-path ".0")))
    
    ;; 更新状态
    (setf (elilog-sink-state sink) `(:current-file ,current-file))
    
    ;; 写入文件
    (let ((formatted (elilog-formatters-format-event event (elilog-sink-formatter sink))))
      (when formatted
        (with-temp-buffer
          (insert formatted "\n")
          (append-to-file (point-min) (point-max) current-file))))))

(defun elilog-sinks--roll-files (base-path max-files)
  "滚动文件。"
  (when max-files
    ;; 删除最老的文件
    (let ((oldest-file (format "%s.%d" base-path (1- max-files))))
      (when (file-exists-p oldest-file)
        (delete-file oldest-file)))
    
    ;; 重命名文件
    (cl-loop for i from (- max-files 2) downto 0 do
             (let ((old-file (format "%s.%d" base-path i))
                   (new-file (format "%s.%d" base-path (1+ i))))
               (when (file-exists-p old-file)
                 (rename-file old-file new-file t))))))

(defun elilog-sinks-create-rolling-file (base-path &optional formatter max-size max-files)
  "创建循环文件Sink。
BASE-PATH: 基础文件路径
FORMATTER: 可选的格式化器
MAX-SIZE: 单个文件最大字节数
MAX-FILES: 最大文件数量"
  (make-elilog-sink
   :name "rolling-file"
   :type 'rolling-file
   :formatter (or formatter (elilog-formatters-create-text))
   :config `(:base-path ,base-path 
            :max-size ,(or max-size (* 10 1024 1024)) ; 默认10MB
            :max-files ,(or max-files 5))              ; 默认5个文件
   :write-fn 'elilog-sinks--write-to-rolling-file
   :state nil))

;;; ===== 网络Sink =====

(defun elilog-sinks--write-to-network (event sink)
  "写入到网络目标（HTTP/TCP）。"
  (let* ((config (elilog-sink-config sink))
         (url (plist-get config :url))
         (method (plist-get config :method))
         (headers (plist-get config :headers))
         (formatted (elilog-formatters-format-event event (elilog-sink-formatter sink))))
    (when formatted
      (condition-case err
          (cond
           ;; HTTP方式
           (url
            (let ((url-request-method (or method "POST"))
                  (url-request-extra-headers headers)
                  (url-request-data formatted))
              (url-retrieve url 
                          (lambda (status)
                            (unless (plist-get status :error)
                              ;; 可以在这里处理响应
                              )))))
           ;; TCP方式（简化实现）
           (t
            (message "Network sink: %s" formatted)))
        (error
         (message "Failed to send log to network: %s" err))))))

(defun elilog-sinks-create-network (url &optional formatter method headers)
  "创建网络Sink。
URL: 目标URL
FORMATTER: 可选的格式化器，建议使用JSON格式化器
METHOD: HTTP方法，默认POST
HEADERS: HTTP头部"
  (make-elilog-sink
   :name "network"
   :type 'network
   :formatter (or formatter (elilog-formatters-create-json))
   :config `(:url ,url :method ,method :headers ,headers)
   :write-fn 'elilog-sinks--write-to-network))

;;; ===== Null Sink =====

(defun elilog-sinks--write-to-null (event sink)
  "空Sink，不执行任何操作。用于测试或禁用输出。"
  nil)

(defun elilog-sinks-create-null ()
  "创建空Sink。"
  (make-elilog-sink
   :name "null"
   :type 'null
   :formatter (elilog-formatters-create-text)
   :write-fn 'elilog-sinks--write-to-null))

;;; ===== Sink实用函数 =====

(defun elilog-sinks-write-event (event sink)
  "向指定Sink写入事件。"
  (when (and event sink (elilog-sink-write-fn sink))
    (funcall (elilog-sink-write-fn sink) event sink)))

(defun elilog-sinks-dispose (sink)
  "释放Sink资源。"
  (when (and sink (elilog-sink-dispose-fn sink))
    (funcall (elilog-sink-dispose-fn sink) sink)))

(defun elilog-sinks-validate (sink)
  "验证Sink是否有效。"
  (and (elilog-sink-p sink)
       (elilog-sink-name sink)
       (elilog-sink-type sink)
       (functionp (elilog-sink-write-fn sink))))

;;; ===== Sink注册表 =====

(defvar elilog-sinks--registry (make-hash-table :test 'equal)
  "Sink注册表。")

(defun elilog-sinks-register (sink)
  "注册Sink到全局注册表。"
  (when (elilog-sinks-validate sink)
    (puthash (elilog-sink-name sink) sink elilog-sinks--registry)
    sink))

(defun elilog-sinks-get (name)
  "从注册表获取Sink。"
  (gethash name elilog-sinks--registry))

(defun elilog-sinks-list ()
  "列出所有已注册的Sink。"
  (let ((sinks nil))
    (maphash (lambda (name sink)
               (push (cons name sink) sinks))
             elilog-sinks--registry)
    sinks))

;;; ===== 组合Sink =====

(defun elilog-sinks--write-to-composite (event sink)
  "写入到组合Sink（将事件发送给多个子Sink）。"
  (let ((child-sinks (plist-get (elilog-sink-config sink) :sinks)))
    (dolist (child-sink child-sinks)
      (elilog-sinks-write-event event child-sink))))

(defun elilog-sinks--dispose-composite (sink)
  "释放组合Sink资源。"
  (let ((child-sinks (plist-get (elilog-sink-config sink) :sinks)))
    (dolist (child-sink child-sinks)
      (elilog-sinks-dispose child-sink))))

(defun elilog-sinks-create-composite (sinks)
  "创建组合Sink。
SINKS: 子Sink列表"
  (make-elilog-sink
   :name "composite"
   :type 'composite
   :formatter nil  ; 由子Sink决定格式化
   :config `(:sinks ,sinks)
   :write-fn 'elilog-sinks--write-to-composite
   :dispose-fn 'elilog-sinks--dispose-composite))

;;; ===== 异步Sink便捷创建函数 =====

(defun elilog-sinks-create-async-file (file-path &optional formatter encoding)
  "创建异步文件Sink（如果异步模块可用）。
如果异步模块不可用，将降级为普通文件Sink。"
  (if (and elilog-sinks--async-available-p 
           (fboundp 'elilog-async-create-file-sink))
      (elilog-async-create-file-sink file-path formatter encoding)
    (elilog-sinks-create-file file-path formatter encoding)))

(defun elilog-sinks-create-batch-file (file-path &optional formatter encoding)
  "创建批量异步文件Sink（如果异步模块可用）。
适合高频日志写入，会自动批量处理以提高性能。"
  (if (and elilog-sinks--async-available-p 
           (fboundp 'elilog-async-create-batch-file-sink))
      (elilog-async-create-batch-file-sink file-path formatter encoding)
    (elilog-sinks-create-file file-path formatter encoding)))

(defun elilog-sinks-create-remote (url &optional formatter headers)
  "创建异步远程Sink（如果异步模块可用）。
如果异步模块不可用，将降级为普通网络Sink。"
  (if (and elilog-sinks--async-available-p 
           (fboundp 'elilog-async-create-remote-sink))
      (elilog-async-create-remote-sink url formatter headers)
    (elilog-sinks-create-network url formatter "POST" headers)))

(defun elilog-sinks-create-smart-file (file-path &optional formatter encoding)
  "创建智能异步文件Sink（如果异步模块可用）。
根据消息复杂度自动选择同步或异步处理。"
  (if (and elilog-sinks--async-available-p 
           (fboundp 'elilog-async-create-smart-file-sink))
      (elilog-async-create-smart-file-sink file-path formatter encoding)
    (elilog-sinks-create-file file-path formatter encoding)))

;;; ===== 异步功能检测 =====

(defun elilog-sinks-async-available-p ()
  "检查异步Sink功能是否可用。"
  elilog-sinks--async-available-p)

(defun elilog-sinks-async-status ()
  "显示异步Sink状态信息。"
  (interactive)
  (if elilog-sinks--async-available-p
      (message "异步Sink功能可用 (emacs-async已加载)")
    (message "异步Sink功能不可用 (需要安装emacs-async包)")))

(provide 'elilog-sinks)

;;; elilog-sinks.el ends here
