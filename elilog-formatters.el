;;; elilog-formatters.el --- Log formatter module  -*- lexical-binding: t; -*-

;; Author: Elilog Development Team
;; Maintainer: Elilog Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: logging, formatters, text, json
;; URL: https://github.com/elilog/elilog

;;; Commentary:
;; This module contains all formatter implementations for the Elilog logging framework.
;; Supports text, JSON, compact, detailed, and custom formatters.
;;
;; 这个模块包含了Elilog日志框架的所有格式化器实现，
;; 支持文本、JSON、紧凑、详细和自定义格式化器。
;;
;; Key Features / 主要特性：
;; - Text formatters with customizable templates / 可自定义模板的文本格式化器
;; - JSON formatters for structured output / 用于结构化输出的JSON格式化器
;; - Compact formatters for space efficiency / 节省空间的紧凑格式化器
;; - Detailed formatters for debugging / 用于调试的详细格式化器
;; - Custom formatters for specialized needs / 用于特殊需求的自定义格式化器
;;
;; Usage Examples / 使用示例：
;; (elilog-formatters-create-text "[{level}] {message}")
;; (elilog-formatters-create-json)
;; (elilog-formatters-create-custom "my-formatter" #'my-format-function)

;;; Code:

(require 'cl-lib)
(require 'json)

;;; ===== Formatter Structure Definition / 格式化器结构定义 =====

(cl-defstruct elilog-formatter
  "Log formatter structure for formatting log events.
日志格式化器结构体，用于格式化日志事件。"
  name                  ; Formatter name / 格式化器名称
  type                  ; Type (text, json, template, etc.) / 类型
  template              ; Template string / 模板字符串
  format-fn)            ; Formatting function / 格式化函数

;;; ===== Built-in Formatter Implementations / 内置格式化器实现 =====

(defun elilog-formatters--format-text (event formatter)
  "Format log event using text formatter.
使用文本格式化器格式化日志事件。"
  (let ((template (elilog-formatter-template formatter))
        (properties (elilog-event-properties event)))
    ;; Replace all placeholders / 替换所有占位符
    (let ((result template))
      (setq result (replace-regexp-in-string "{timestamp}" 
                                           (or (elilog-event-timestamp event) "") 
                                           result t t))
      (setq result (replace-regexp-in-string "{level}" 
                                           (upcase (symbol-name (elilog-event-level event))) 
                                           result t t))
      (setq result (replace-regexp-in-string "{logger-name}" 
                                           (or (elilog-event-logger-name event) "") 
                                           result t t))
      (setq result (replace-regexp-in-string "{message}" 
                                           (or (elilog-event-message event) "") 
                                           result t t))
      (setq result (replace-regexp-in-string "{thread-id}" 
                                           (or (elilog-event-thread-id event) "") 
                                           result t t))
      ;; Handle properties placeholder / 处理属性占位符
      (when properties
        (let ((props-str (format "%S" properties)))
          (setq result (replace-regexp-in-string "{properties}" props-str result t t))))
      result)))

(defun elilog-formatters--format-json (event formatter)
  "Format log event using JSON formatter.
使用JSON格式化器格式化日志事件。

EVENT: The log event to format / 要格式化的日志事件
FORMATTER: The JSON formatter instance / JSON格式化器实例

Returns a JSON string representation of the log event.
返回日志事件的JSON字符串表示。"
  (json-encode
   `((timestamp . ,(elilog-event-timestamp event))
     (level . ,(symbol-name (elilog-event-level event)))
     (message . ,(elilog-event-message event))
     (logger . ,(elilog-event-logger-name event))
     (thread . ,(elilog-event-thread-id event))
     (properties . ,(elilog-event-properties event))
     (exception . ,(elilog-event-exception event)))))

(defun elilog-formatters--format-compact (event formatter)
  "Format log event using compact formatter for single-line output.
使用紧凑格式化器格式化日志事件，单行输出。

EVENT: The log event to format / 要格式化的日志事件
FORMATTER: The compact formatter instance / 紧凑格式化器实例

Returns a compact, single-line string representation.
返回紧凑的单行字符串表示。"
  (let ((props (elilog-event-properties event)))
    (format "%s [%s] %s%s"
            (elilog-event-timestamp event)
            (upcase (symbol-name (elilog-event-level event)))
            (elilog-event-message event)
            (if props (format " %S" props) ""))))

(defun elilog-formatters--format-detailed (event formatter)
  "Format log event using detailed formatter for multi-line output.
使用详细格式化器格式化日志事件，多行输出。

EVENT: The log event to format / 要格式化的日志事件
FORMATTER: The detailed formatter instance / 详细格式化器实例

Returns a detailed, multi-line string representation for debugging.
返回用于调试的详细多行字符串表示。"
  (let ((props (elilog-event-properties event))
        (exception (elilog-event-exception event)))
    (concat
     (format "=== LOG ENTRY ===\n")
     (format "Timestamp: %s\n" (elilog-event-timestamp event))
     (format "Level:     %s\n" (upcase (symbol-name (elilog-event-level event))))
     (format "Logger:    %s\n" (or (elilog-event-logger-name event) "default"))
     (format "Thread:    %s\n" (or (elilog-event-thread-id event) "main"))
     (format "Message:   %s\n" (elilog-event-message event))
     (when props
       (format "Properties: %S\n" props))
     (when exception
       (format "Exception: %S\n" exception))
     "==================")))

;;; ===== 格式化器创建函数 =====

(defun elilog-formatters-create-text (&optional template)
  "Create a text formatter with customizable template.
创建可自定义模板的文本格式化器。

TEMPLATE: Optional template string supporting the following placeholders:
         可选的模板字符串，支持以下占位符：
- {timestamp}: Event timestamp / 事件时间戳
- {level}: Log level / 日志级别
- {logger-name}: Logger name / 记录器名称
- {message}: Log message / 日志消息
- {thread-id}: Thread ID / 线程ID
- {properties}: Property list / 属性列表

Returns an elilog-formatter structure.
返回 elilog-formatter 结构体。"
  (make-elilog-formatter
   :name "text"
   :type 'text
   :template (or template "[{timestamp}] [{level}] {logger-name}: {message}")
   :format-fn 'elilog-formatters--format-text))

(defun elilog-formatters-create-json ()
  "Create a JSON formatter for structured output.
创建用于结构化输出的JSON格式化器。

Returns an elilog-formatter structure that outputs JSON.
返回输出JSON的 elilog-formatter 结构体。"
  (make-elilog-formatter
   :name "json"
   :type 'json
   :format-fn 'elilog-formatters--format-json))

(defun elilog-formatters-create-compact ()
  "Create a compact formatter for space-efficient output.
创建节省空间的紧凑格式化器。

Returns an elilog-formatter structure for single-line output.
返回用于单行输出的 elilog-formatter 结构体。"
  (make-elilog-formatter
   :name "compact"
   :type 'compact
   :format-fn 'elilog-formatters--format-compact))

(defun elilog-formatters-create-detailed ()
  "Create a detailed formatter for debugging and comprehensive output.
创建用于调试和全面输出的详细格式化器。

Returns an elilog-formatter structure for multi-line detailed output.
返回用于多行详细输出的 elilog-formatter 结构体。"
  (make-elilog-formatter
   :name "detailed"
   :type 'detailed
   :format-fn 'elilog-formatters--format-detailed))

(defun elilog-formatters-create-custom (name format-fn)
  "Create a custom formatter with user-defined formatting function.
创建带有用户定义格式化函数的自定义格式化器。

NAME: Name of the formatter / 格式化器名称
FORMAT-FN: Formatting function that takes (event formatter) as parameters
          格式化函数，接受 (event formatter) 两个参数

Returns an elilog-formatter structure with custom formatting logic.
返回带有自定义格式化逻辑的 elilog-formatter 结构体。"
  (make-elilog-formatter
   :name name
   :type 'custom
   :format-fn format-fn))

;;; ===== 格式化器实用函数 =====

(defun elilog-formatters-format-event (event formatter)
  "Format a log event using the specified formatter.
使用指定格式化器格式化日志事件。

EVENT: The log event to format / 要格式化的日志事件
FORMATTER: The formatter to use / 要使用的格式化器

Returns the formatted string or nil if formatting fails.
返回格式化的字符串，如果格式化失败则返回 nil。"
  (when (and event formatter (elilog-formatter-format-fn formatter))
    (funcall (elilog-formatter-format-fn formatter) event formatter)))

(defun elilog-formatters-validate (formatter)
  "Validate if formatter is valid.
验证格式化器是否有效。"
  (and (elilog-formatter-p formatter)
       (elilog-formatter-name formatter)
       (elilog-formatter-type formatter)
       (functionp (elilog-formatter-format-fn formatter))))

;;; ===== 预定义格式化器注册表 =====

(defvar elilog-formatters--registry (make-hash-table :test 'equal)
  "格式化器注册表。")

(defun elilog-formatters-register (formatter)
  "Register formatter to the global registry.
注册格式化器到全局注册表。

FORMATTER: The formatter to register / 要注册的格式化器

Returns the formatter if registration succeeds, nil otherwise.
如果注册成功返回格式化器，否则返回 nil。"
  (when (elilog-formatters-validate formatter)
    (puthash (elilog-formatter-name formatter) formatter elilog-formatters--registry)
    formatter))

(defun elilog-formatters-get (name)
  "Get formatter from the registry by name.
从注册表按名称获取格式化器。

NAME: Name of the formatter to retrieve / 要检索的格式化器名称

Returns the formatter if found, nil otherwise.
如果找到返回格式化器，否则返回 nil。"
  (gethash name elilog-formatters--registry))

(defun elilog-formatters-list ()
  "List all registered formatters.
列出所有已注册的格式化器。

Returns an alist of (name . formatter) pairs.
返回 (name . formatter) 对的关联列表。"
  (let ((formatters nil))
    (maphash (lambda (name formatter)
               (push (cons name formatter) formatters))
             elilog-formatters--registry)
    formatters))

;;; ===== 初始化预定义格式化器 =====

(defun elilog-formatters-initialize ()
  "Initialize and register predefined formatters.
初始化并注册预定义的格式化器。

This function registers all built-in formatters to the global registry.
此函数将所有内置格式化器注册到全局注册表。"
  (elilog-formatters-register (elilog-formatters-create-text))
  (elilog-formatters-register (elilog-formatters-create-json))
  (elilog-formatters-register (elilog-formatters-create-compact))
  (elilog-formatters-register (elilog-formatters-create-detailed)))

;; 自动初始化
(elilog-formatters-initialize)

(provide 'elilog-formatters)

;;; elilog-formatters.el ends here
