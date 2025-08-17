;;; elilog.el --- A Serilog-inspired logging framework for Emacs Lisp  -*- lexical-binding: t; -*-

;;; Commentary:
;; Elilog is a Serilog-inspired logging framework for Emacs Lisp that provides
;; structured logging, multiple output targets, and flexible configuration.
;;
;; Elilog 是一个受 Serilog 启发的 Emacs Lisp 日志框架，提供结构化日志记录、
;; 多种输出目标和灵活的配置功能。
;; 
;; Key Features / 主要特性：
;; - Structured logging with property lists / 使用属性列表的结构化日志记录
;; - Multi-level logging system (Trace, Debug, Info, Warning, Error, Fatal) / 多级别日志系统
;; - Multiple output targets (console, file, buffer, remote) / 多种输出目标
;; - Flexible formatting system (JSON, text, custom templates) / 灵活的格式化系统
;; - Powerful filtering system / 强大的过滤器系统
;; - Logging context management / 日志上下文管理
;; - Asynchronous logging support / 异步日志写入支持
;;
;; Usage Examples / 使用示例：
;; (elilog-info "User login" :user-id 123 :ip "192.168.1.1")
;; (elilog-error "Database connection failed" :error-code 500 :database "main")

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elilog-formatters)
(require 'elilog-sinks)

;; Optional async module loading / 可选加载异步模块
(defvar elilog-async-module-loaded-p
  (condition-case nil
      (progn (require 'elilog-async) t)
    (error nil))
  "Check if async module is loaded.
检查异步模块是否已加载。")

;;; ===== Core Configuration and Constants / 核心配置和常量 =====

(defgroup elilog nil
  "Elilog logging framework configuration group.
Elilog 日志框架配置组。"
  :group 'tools
  :prefix "elilog-")

;; Log level definitions / 日志级别定义
(defconst elilog-levels
  '((trace . 0)    ; Most verbose / 最详细
    (debug . 10)   ; Debug information / 调试信息
    (info . 20)    ; General information / 一般信息
    (warning . 30) ; Warning messages / 警告消息
    (error . 40)   ; Error messages / 错误消息
    (fatal . 50))  ; Critical errors / 严重错误
  "Log level definitions with numeric values, higher values indicate higher priority.
日志级别定义，数值越大级别越高。")

(defcustom elilog-minimum-level 'info
  "Minimum log level, logs below this level will be ignored.
最小日志级别，低于此级别的日志将被忽略。"
  :type '(choice (const :tag "Trace" trace)
                 (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warning)
                 (const :tag "Error" error)
                 (const :tag "Fatal" fatal))
  :group 'elilog)

(defcustom elilog-time-format "%Y-%m-%d %H:%M:%S.%3N"
  "Timestamp format for log entries.
日志时间戳格式。"
  :type 'string
  :group 'elilog)


;;; ===== Core Data Structures / 核心数据结构 =====

;; Log event structure / 日志事件结构
(cl-defstruct elilog-event
  "Log event structure containing all event information.
日志事件结构体，包含所有事件信息。"
  timestamp               ; Event timestamp / 事件时间戳
  level                  ; Log level / 日志级别
  message               ; Log message / 日志消息
  properties            ; Property list (structured data) / 属性列表（结构化数据）
  exception             ; Exception information / 异常信息
  logger-name           ; Logger name / 记录器名称
  thread-id             ; Thread ID (buffer name in Emacs) / 线程ID（在Emacs中为buffer名）
  context)              ; Context information / 上下文信息

;; Filter structure / 过滤器结构
(cl-defstruct elilog-filter
  "Log filter structure for filtering log events.
日志过滤器结构体，用于过滤日志事件。"
  name                  ; Filter name / 过滤器名称
  predicate             ; Predicate function / 谓词函数
  config)               ; Configuration / 配置信息

;; Logger structure / 日志记录器结构
(cl-defstruct elilog-logger
  "Logger structure that manages log processing.
日志记录器结构体，管理日志处理。"
  name                  ; Logger name / 记录器名称
  level                 ; Minimum log level / 最小日志级别
  sinks                 ; Output target list / 输出目标列表
  context               ; Static context / 静态上下文信息
  parent                ; Parent logger / 父记录器
  children              ; Child loggers / 子记录器列表
  enabled)              ; Whether enabled / 是否启用

;;; ===== Global Variables / 全局变量 =====

;; Logger registry / 记录器注册表
(defvar elilog-loggers (make-hash-table :test 'equal)
  "Hash table of registered loggers.
注册的日志记录器哈希表。")

;; Filter registry / 过滤器注册表
(defvar elilog-filters (make-hash-table :test 'equal)
  "Hash table of registered filters.
注册的过滤器哈希表。")

;; Global context / 全局上下文
(defvar elilog-global-context nil
  "Global logging context property list.
全局日志上下文属性列表。")

;;; ===== Utility Functions / 工具函数 =====

(defun elilog--get-timestamp ()
  "Get current timestamp string.
获取当前时间戳字符串。"
  (format-time-string elilog-time-format))

(defun elilog--level-numeric (level)
  "Get numeric value for log level.
获取日志级别的数值。"
  (or (cdr (assq level elilog-levels)) 0))

(defun elilog--should-log-p (logger-level event-level)
  "Determine if event should be logged based on levels.
根据级别判断是否应该记录日志。"
  (>= (elilog--level-numeric event-level)
      (elilog--level-numeric logger-level)))

(defun elilog--merge-properties (&rest property-lists)
  "Merge multiple property lists into one.
合并多个属性列表。"
  (let ((result nil))
    (dolist (plist property-lists)
      (when plist
        (let ((i 0))
          (while (< i (length plist))
            (setq result (plist-put result (nth i plist) (nth (1+ i) plist)))
            (setq i (+ i 2))))))
    result))

(defun elilog--current-thread-id ()
  "Get current thread ID (buffer name in Emacs).
获取当前线程ID（在Emacs中使用buffer名称）。"
  (if (buffer-name)
      (buffer-name)
    "main"))



;;; ===== Filter Implementation / 过滤器实现 =====

(defun elilog--create-level-filter (min-level)
  "Create a level filter.
创建级别过滤器。"
  (make-elilog-filter
   :name "level"
   :predicate (lambda (event)
                (elilog--should-log-p min-level (elilog-event-level event)))
   :config `(:min-level ,min-level)))

(defun elilog--create-property-filter (property value)
  "Create a property filter.
创建属性过滤器。"
  (make-elilog-filter
   :name "property"
   :predicate (lambda (event)
                (equal (plist-get (elilog-event-properties event) property) value))
   :config `(:property ,property :value ,value)))

(defun elilog--create-lambda-filter (predicate-fn)
  "Create a custom lambda filter.
创建自定义Lambda过滤器。"
  (make-elilog-filter
   :name "lambda"
   :predicate predicate-fn))

(defun elilog--apply-filters (event filters)
  "Apply filter list to log event.
对日志事件应用过滤器列表。"
  (cl-every (lambda (filter)
             (funcall (elilog-filter-predicate filter) event))
           filters))

;;; ===== Logger Management / 记录器管理 =====

(defun elilog--create-logger (name &optional level sinks context)
  "Create a new logger.
创建新的日志记录器。"
  (make-elilog-logger
   :name name
   :level (or level elilog-minimum-level)
   :sinks (or sinks (list (elilog-sinks-create-console)))
   :context context
   :enabled t))

(defun elilog-register-logger (logger)
  "Register a logger.
注册日志记录器。"
  (puthash (elilog-logger-name logger) logger elilog-loggers))

(defun elilog-get-logger (name)
  "Get logger by name.
获取指定名称的日志记录器。"
  (gethash name elilog-loggers))

(defun elilog-get-or-create-logger (name)
  "Get existing logger or create new one.
获取或创建指定名称的日志记录器。"
  (or (elilog-get-logger name)
      (let ((logger (elilog--create-logger name)))
        (elilog-register-logger logger)
        logger)))

;; Create and register default logger / 创建并注册默认记录器
(defvar elilog-default-logger
  (elilog--create-logger "default" elilog-minimum-level)
  "Default global logger.
默认的全局日志记录器。")

(elilog-register-logger elilog-default-logger)

;;; ===== Core Logging Functions / 核心日志记录函数 =====

(defun elilog--create-event (level message &rest properties)
  "Create a log event.
创建日志事件。"
  (make-elilog-event
   :timestamp (elilog--get-timestamp)
   :level level
   :message message
   :properties (elilog--merge-properties elilog-global-context properties)
   :logger-name (elilog-logger-name elilog-default-logger)
   :thread-id (elilog--current-thread-id)))

(defun elilog--write-event (event logger)
  "Write log event to all sinks of the logger.
将日志事件写入到记录器的所有Sink。"
  (when (and (elilog-logger-enabled logger)
             (elilog--should-log-p (elilog-logger-level logger) 
                                   (elilog-event-level event)))
    (dolist (sink (elilog-logger-sinks logger))
      (when (or (not (elilog-sink-filter sink))
                (elilog--apply-filters event (list (elilog-sink-filter sink))))
        (elilog-sinks-write-event event sink)))))

(defun elilog--log-with-logger (logger level message &rest properties)
  "Log with specified logger.
使用指定记录器记录日志。"
  (let ((event (apply 'elilog--create-event level message properties)))
    (setf (elilog-event-logger-name event) (elilog-logger-name logger))
    (elilog--write-event event logger)))

(defun elilog-log (level message &rest properties)
  "Log with default logger.
使用默认记录器记录日志。"
  (apply 'elilog--log-with-logger elilog-default-logger level message properties))

;;; ===== Convenience API Functions / 便捷API函数 =====

(defun elilog-trace (message &rest properties)
  "Log trace level message.
记录Trace级别日志。"
  (apply 'elilog-log 'trace message properties))

(defun elilog-debug (message &rest properties)
  "Log debug level message.
记录Debug级别日志。"
  (apply 'elilog-log 'debug message properties))

(defun elilog-info (message &rest properties)
  "Log info level message.
记录Info级别日志。"
  (apply 'elilog-log 'info message properties))

(defun elilog-warning (message &rest properties)
  "Log warning level message.
记录Warning级别日志。"
  (apply 'elilog-log 'warning message properties))

(defun elilog-error (message &rest properties)
  "Log error level message.
记录Error级别日志。"
  (apply 'elilog-log 'error message properties))

(defun elilog-fatal (message &rest properties)
  "Log fatal level message.
记录Fatal级别日志。"
  (apply 'elilog-log 'fatal message properties))

;;; ===== Context Management / 上下文管理 =====

(defmacro elilog-with-context-macro (context-properties &rest body)
  "Execute code with specified context properties.
在指定上下文中执行代码。

CONTEXT-PROPERTIES: Property list to add to context / 要添加到上下文的属性列表
BODY: Code to execute with context / 在上下文中执行的代码

This is the macro version for compile-time optimization.
这是用于编译时优化的宏版本。"
  `(let ((original-context elilog-global-context))
     (unwind-protect
         (progn
           (setq elilog-global-context
                 (elilog--merge-properties elilog-global-context ,context-properties))
           ,@body)
       (setq elilog-global-context original-context))))

(defun elilog-with-context (context-properties function &rest args)
  "Execute function with specified context properties (function version).
在指定上下文属性中执行函数（函数版本）。

CONTEXT-PROPERTIES: Property list to add to context / 要添加到上下文的属性列表
FUNCTION: Function to call with context / 在上下文中调用的函数
ARGS: Arguments to pass to the function / 传递给函数的参数

This is the function version that doesn't require macros.
这是不需要宏的函数版本。

Returns the result of calling FUNCTION with ARGS.
返回调用 FUNCTION 与 ARGS 的结果。

Example / 示例:
  (elilog-with-context '(:request-id \"12345\" :user \"admin\")
                       (lambda () (elilog-info \"Operation completed\")))
  
  (elilog-with-context '(:transaction-id \"tx-001\")
                       #'my-business-function
                       arg1 arg2)"
  (let ((original-context elilog-global-context))
    (unwind-protect
        (progn
          (setq elilog-global-context
                (elilog--merge-properties elilog-global-context context-properties))
          (apply function args))
      (setq elilog-global-context original-context))))

(defun elilog-push-context (context-properties)
  "Push context properties to global context.
推送上下文属性到全局上下文。"
  (setq elilog-global-context 
        (elilog--merge-properties elilog-global-context context-properties)))

(defun elilog-pop-context ()
  "Clear global context.
清空全局上下文。"
  (setq elilog-global-context nil))

;;; ===== Configuration and Management API / 配置和管理API =====

(defun elilog-configure-logger (logger-name &rest options)
  "Configure the specified logger.
配置指定的日志记录器。

Options / 选项：
:level - Set minimum log level / 设置最小日志级别
:sinks - Set output target list / 设置输出目标列表  
:context - Set static context / 设置静态上下文
:enabled - Enable or disable logger / 启用或禁用记录器"
  (let ((logger (elilog-get-or-create-logger logger-name)))
    (when (plist-get options :level)
      (setf (elilog-logger-level logger) (plist-get options :level)))
    (when (plist-get options :sinks)
      (setf (elilog-logger-sinks logger) (plist-get options :sinks)))
    (when (plist-get options :context)
      (setf (elilog-logger-context logger) (plist-get options :context)))
    (when (plist-member options :enabled)
      (setf (elilog-logger-enabled logger) (plist-get options :enabled)))
    logger))

(defun elilog-add-sink (logger-name sink)
  "Add sink to specified logger.
向指定记录器添加Sink。"
  (let ((logger (elilog-get-or-create-logger logger-name)))
    (push sink (elilog-logger-sinks logger))))

(defun elilog-remove-sink (logger-name sink-name)
  "Remove sink from specified logger.
从指定记录器移除Sink。"
  (let ((logger (elilog-get-logger logger-name)))
    (when logger
      (setf (elilog-logger-sinks logger)
            (cl-remove-if (lambda (sink)
                           (string= (elilog-sink-name sink) sink-name))
                         (elilog-logger-sinks logger))))))

(defun elilog-set-global-level (level)
  "Set global minimum log level.
设置全局最小日志级别。"
  (setq elilog-minimum-level level)
  (setf (elilog-logger-level elilog-default-logger) level))

;;; ===== Initialization and Cleanup / 初始化和清理 =====

(defun elilog-initialize ()
  "Initialize the logging framework.
初始化日志框架。"
  (when elilog-async-module-loaded-p
    (message "Elilog: Async module loaded / 异步模块已加载")))

(defun elilog-init (&optional level sinks)
  "Quick initialization function for users.
用户快速初始化函数。

LEVEL: Optional global log level (default: 'info) / 可选的全局日志级别（默认：'info）
SINKS: Optional list of sinks for default logger / 默认记录器的可选输出目标列表

This function provides a simple way to initialize elilog for basic usage.
此函数提供了为基本使用初始化 elilog 的简单方法。"
  (interactive)
  (when level
    (elilog-set-global-level level))
  (when sinks
    (setf (elilog-logger-sinks elilog-default-logger) sinks))
  (elilog-initialize)
  (elilog-info "Elilog initialized" :level (or level elilog-minimum-level)))

(defun elilog-shutdown ()
  "Shutdown the logging framework and cleanup resources.
关闭日志框架并清理资源。"
  ;; Call async module shutdown function / 调用异步模块的关闭函数
  (when (fboundp 'elilog-async-shutdown)
    (elilog-async-shutdown))
  ;; Call cleanup functions for all sinks / 调用所有Sink的清理函数
  (maphash (lambda (name logger)
             (dolist (sink (elilog-logger-sinks logger))
               (elilog-sinks-dispose sink)))
           elilog-loggers))

;; Auto cleanup on Emacs exit / 在Emacs退出时自动清理
(add-hook 'kill-emacs-hook 'elilog-shutdown)

(provide 'elilog)

;;; elilog.el ends here
