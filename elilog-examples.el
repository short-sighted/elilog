;;; elilog-examples.el --- Elilog logging framework usage examples  -*- lexical-binding: t; -*-

;; Author: Elilog Development Team
;; Maintainer: Elilog Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: logging, examples, tutorial, demonstration
;; URL: https://github.com/elilog/elilog

;;; Commentary:
;; This file contains comprehensive usage examples for the Elilog logging framework.
;; It demonstrates various features and usage patterns to help users get started.
;;
;; 这个文件包含了 Elilog 日志框架的全面使用示例，
;; 演示了各种功能和使用模式，帮助用户快速上手。
;;
;; Key Features Demonstrated / 演示的主要功能:
;; - Basic logging operations / 基本日志操作
;; - Structured logging with properties / 带属性的结构化日志
;; - Context management / 上下文管理
;; - Custom formatters and sinks / 自定义格式化器和输出目标
;; - Asynchronous logging / 异步日志记录
;; - Performance optimization / 性能优化
;; - Integration patterns / 集成模式
;;
;; Usage / 使用方法:
;; Load this file and run the example functions interactively:
;; 加载此文件并交互式运行示例函数：
;; M-x elilog-example-basic-usage
;; M-x elilog-run-all-examples

;;; Code:

(require 'cl-lib)
(require 'elilog)

;;; ===== 基础使用示例 =====

(defun elilog-example-basic-usage ()
  "基础日志记录示例。"
  (interactive)
  (message "=== 基础日志记录示例 ===")
  
  ;; 基本日志记录
  (elilog-info "应用程序启动")
  (elilog-debug "调试信息" :module "main" :version "1.0.0")
  (elilog-warning "这是一个警告")
  (elilog-error "发生了错误" :error-code 404 :details "文件未找到")
  
  ;; 结构化日志记录
  (elilog-info "用户操作" 
               :user-id 12345
               :action "login"
               :ip-address "192.168.1.100"
               :timestamp (current-time-string))
  
  (elilog-error "数据库连接失败"
                :database "main-db"
                :host "localhost"
                :port 5432
                :retry-count 3))

;;; ===== 多Sink示例 =====

(defun elilog-example-multiple-sinks ()
  "多种输出目标示例。"
  (interactive)
  (message "=== 多Sink输出示例 ===")
  
  ;; 创建文件Sink
  (let ((file-sink (elilog-sinks-create-file "/tmp/elilog-test.log"))
        (buffer-sink (elilog-sinks-create-buffer "*elilog-test*"))
        (json-file-sink (elilog-sinks-create-file "/tmp/elilog-test.json" 
                                                  (elilog-formatters-create-json))))
    
    ;; 配置记录器使用多个Sink
    (elilog-configure-logger "multi-sink-test"
                            :level 'debug
                            :sinks (list file-sink buffer-sink json-file-sink))
    
    ;; 使用配置的记录器记录日志
    (let ((logger (elilog-get-logger "multi-sink-test")))
      (elilog--log-with-logger logger 'info "这条日志会同时输出到文件、缓冲区和JSON文件"
                              :test-id 1
                              :data "测试数据")
      (elilog--log-with-logger logger 'debug "调试信息"
                              :debug-level "verbose")
      (elilog--log-with-logger logger 'warning "警告信息"
                              :component "data-processor"))))

;;; ===== 上下文管理示例 =====

(defun elilog-example-context-management ()
  "上下文管理示例。"
  (interactive)
  (message "=== 上下文管理示例 ===")
  
  ;; 设置全局上下文
  (elilog-push-context '(:application "MyApp" :environment "development"))
  
  (elilog-info "应用启动" :startup-time (current-time-string))
  
  ;; 使用临时上下文
  (elilog-with-context-macro '(:request-id "req-123" :user-id 456)
    (elilog-info "处理用户请求")
    (elilog-debug "验证用户权限")
    (elilog-info "请求处理完成"))
  
  ;; 嵌套上下文
  (elilog-with-context-macro '(:module "payment")
    (elilog-info "开始支付处理")
    (elilog-with-context-macro '(:payment-id "pay-789" :amount 99.99)
      (elilog-info "验证支付信息")
      (elilog-debug "调用支付网关")
      (elilog-info "支付处理成功"))
    (elilog-info "支付流程结束"))
  
  ;; 清除全局上下文
  (elilog-pop-context))

;;; ===== 过滤器示例 =====

(defun elilog-example-filters ()
  "过滤器示例。"
  (interactive)
  (message "=== 过滤器示例 ===")
  
  ;; 创建带过滤器的Sink
  (let ((filtered-sink (elilog-sinks-create-console))
        (level-filter (elilog--create-level-filter 'warning))
        (property-filter (elilog--create-property-filter :important t)))
    
    ;; 为Sink设置级别过滤器（只记录Warning及以上级别）
    (setf (elilog-sink-filter filtered-sink) level-filter)
    
    (elilog-configure-logger "filtered-test"
                            :level 'trace
                            :sinks (list filtered-sink))
    
    (let ((logger (elilog-get-logger "filtered-test")))
      (message "以下日志只有Warning和Error会被输出:")
      (elilog--log-with-logger logger 'debug "这条调试信息不会显示")
      (elilog--log-with-logger logger 'info "这条信息不会显示")
      (elilog--log-with-logger logger 'warning "这条警告会显示")
      (elilog--log-with-logger logger 'error "这条错误会显示"))
    
    ;; 创建属性过滤器示例
    (let ((property-sink (elilog-sinks-create-console)))
      (setf (elilog-sink-filter property-sink) property-filter)
      
      (elilog-configure-logger "property-filtered-test"
                              :level 'trace
                              :sinks (list property-sink))
      
      (let ((logger (elilog-get-logger "property-filtered-test")))
        (message "以下日志只有标记为重要的会被输出:")
        (elilog--log-with-logger logger 'info "普通日志" :important nil)
        (elilog--log-with-logger logger 'info "重要日志" :important t)
        (elilog--log-with-logger logger 'error "普通错误")
        (elilog--log-with-logger logger 'error "重要错误" :important t)))))

;;; ===== 异步写入示例 =====

(defun elilog-example-async-logging ()
  "新版异步日志写入示例。"
  (interactive)
  (message "=== 新版异步日志写入示例 ===")
  
  ;; 检查异步模块是否可用
  (if (elilog-sinks-async-available-p)
      (progn
        (message "异步模块可用，使用emacs-async进行异步处理")
        
        ;; 创建异步文件Sink
        (let ((async-sink (elilog-sinks-create-async-file "/tmp/elilog-async-new.log"))
              (batch-sink (elilog-sinks-create-batch-file "/tmp/elilog-batch-new.log"))
              (smart-sink (elilog-sinks-create-smart-file "/tmp/elilog-smart-new.log")))
          
          (elilog-configure-logger "async-demo"
                                  :level 'debug
                                  :sinks (list async-sink batch-sink smart-sink))
          
          (let ((logger (elilog-get-logger "async-demo")))
            (message "开始新版异步日志写入测试...")
            
            ;; 快速生成大量日志
            (dotimes (i 50)
              (elilog--log-with-logger logger 'info (format "新异步日志 #%d" i)
                                      :batch-id "async-demo-1"
                                      :index i
                                      :async-test t))
            
            ;; 生成一些复杂日志（会触发异步处理）
            (dotimes (i 10)
              (elilog--log-with-logger logger 'error 
                                      (format "复杂错误日志 #%d，包含大量信息用于测试异步处理性能和正确性" i)
                                      :error-code (+ 1000 i)
                                      :stack-trace (format "at function_%d line_%d" i (* i 10))
                                      :user-data (cl-loop for j from 1 to 5
                                                         collect (cons (format "key%d" j) 
                                                                      (format "value%d_%d" i j)))))
            
            (message "已提交60条日志（包含50条普通日志和10条复杂日志）")
            (message "检查以下文件查看异步写入结果：")
            (message "- /tmp/elilog-async-new.log (异步文件)")
            (message "- /tmp/elilog-batch-new.log (批量异步)")  
            (message "- /tmp/elilog-smart-new.log (智能异步)")
            
            ;; 显示异步统计信息
            (when (fboundp 'elilog-async-statistics)
              (run-with-timer 2 nil (lambda ()
                                     (let ((stats (elilog-async-statistics)))
                                       (message "异步统计: %s" stats))))))))
    
    ;; 降级为旧版异步实现或同步实现
    (message "异步模块不可用，使用同步处理")
    (let ((sync-sink (elilog-sinks-create-file "/tmp/elilog-sync-fallback.log")))
      (elilog-configure-logger "sync-fallback"
                              :level 'debug
                              :sinks (list sync-sink))
      
      (let ((logger (elilog-get-logger "sync-fallback")))
        (dotimes (i 20)
          (elilog--log-with-logger logger 'info (format "同步日志 #%d" i)
                                  :fallback t
                                  :index i))
        (message "已写入20条同步日志到 /tmp/elilog-sync-fallback.log")))))

;;; ===== 自定义格式化器示例 =====

(defun elilog-example-custom-formatter ()
  "自定义格式化器示例。"
  (interactive)
  (message "=== 自定义格式化器示例 ===")
  
  ;; 创建自定义格式化器
  (let ((custom-formatter 
         (make-elilog-formatter
          :name "custom"
          :type 'custom
          :format-fn (lambda (event formatter)
                      (format "🚀 %s | %s | %s | %s"
                              (upcase (symbol-name (elilog-event-level event)))
                              (elilog-event-timestamp event)
                              (elilog-event-message event)
                              (if (elilog-event-properties event)
                                  (format "Props: %s" (elilog-event-properties event))
                                "No props"))))))
    
    ;; 创建使用自定义格式化器的Sink
    (let ((custom-sink (elilog-sinks-create-console custom-formatter)))
      (elilog-configure-logger "custom-format-test"
                              :level 'debug
                              :sinks (list custom-sink))
      
      (let ((logger (elilog-get-logger "custom-format-test")))
        (elilog--log-with-logger logger 'info "自定义格式化的日志")
        (elilog--log-with-logger logger 'warning "带属性的日志" 
                                :user "张三" :action "登录")
        (elilog--log-with-logger logger 'error "错误日志" 
                                :error-type "validation" :field "email")))))

;;; ===== 高级异步功能演示 =====

(defun elilog-example-advanced-async ()
  "高级异步功能演示。"
  (interactive)
  (message "=== 高级异步功能演示 ===")
  
  (when (elilog-sinks-async-available-p)
    ;; 演示不同类型的异步Sink
    (let ((high-perf-sink (elilog-sinks-create-batch-file "/tmp/high-perf.log"
                                                          (elilog-formatters-create-json)))
          (smart-sink (elilog-sinks-create-smart-file "/tmp/smart.log"))
          (remote-sink (elilog-sinks-create-remote "http://httpbin.org/post"
                                                   (elilog-formatters-create-json)
                                                   '(("Content-Type" . "application/json")))))
      
      ;; 配置高性能记录器
      (elilog-configure-logger "high-performance"
                              :level 'debug
                              :sinks (list high-perf-sink smart-sink))
      
      ;; 配置远程记录器  
      (elilog-configure-logger "remote-logging"
                              :level 'error
                              :sinks (list remote-sink))
      
      (let ((perf-logger (elilog-get-logger "high-performance"))
            (remote-logger (elilog-get-logger "remote-logging")))
        
        ;; 高频日志写入测试
        (message "开始高频日志写入测试...")
        (dotimes (i 200)
          (elilog--log-with-logger perf-logger 'info 
                                  (format "高频日志 #%d" i)
                                  :timestamp (current-time-string)
                                  :batch-id "high-perf-test"
                                  :data (make-string 100 ?x))) ; 较长的数据
        
        ;; 远程日志发送测试
        (message "开始远程日志发送测试...")
        (elilog--log-with-logger remote-logger 'error 
                                "远程错误报告"
                                :application "elilog-demo"
                                :error-type "network"
                                :severity "high"
                                :details "This is a test error sent to remote server")
        
        ;; 显示异步统计
        (when (fboundp 'elilog-async-statistics)
          (run-with-timer 3 nil (lambda ()
                                 (message "最终异步统计: %s" 
                                         (elilog-async-statistics)))))
        
        (message "高级异步演示完成！")
        (message "检查文件: /tmp/high-perf.log (批量异步JSON)")
        (message "检查文件: /tmp/smart.log (智能异步文本)")
        (message "远程日志已发送到 httpbin.org (如果网络可用)")))))

;;; ===== 性能测试 =====

(defun elilog-performance-test ()
  "性能测试。"
  (interactive)
  (message "=== 性能测试 ===")
  
  (let ((count 1000))
    ;; 同步模式性能测试
    (message "测试同步模式性能...")
    (let ((sync-sink (elilog-sinks-create-file "/tmp/perf-sync.log"))
          (start-time (current-time)))
      
      (elilog-configure-logger "perf-sync"
                              :level 'debug
                              :sinks (list sync-sink))
      
      (let ((logger (elilog-get-logger "perf-sync")))
        (dotimes (i count)
          (elilog--log-with-logger logger 'info (format "同步性能测试日志 #%d" i)
                                  :test-id i
                                  :batch "sync-test")))
      
      (let ((sync-time (float-time (time-subtract (current-time) start-time))))
        (message "同步模式：%d条日志，耗时 %.3f秒，平均 %.3f条/秒" 
                 count sync-time (/ count sync-time))))
    
    ;; 异步模式性能测试
    (when (elilog-sinks-async-available-p)
      (message "测试异步模式性能...")
      (let ((async-sink (elilog-sinks-create-async-file "/tmp/perf-async.log"))
            (batch-sink (elilog-sinks-create-batch-file "/tmp/perf-batch.log"))
            (start-time (current-time)))
        
        (elilog-configure-logger "perf-async"
                                :level 'debug
                                :sinks (list async-sink))
        
        (elilog-configure-logger "perf-batch"
                                :level 'debug
                                :sinks (list batch-sink))
        
        (let ((async-logger (elilog-get-logger "perf-async"))
              (batch-logger (elilog-get-logger "perf-batch")))
          
          ;; 异步单条写入测试
          (dotimes (i (/ count 2))
            (elilog--log-with-logger async-logger 'info 
                                    (format "异步性能测试日志 #%d" i)
                                    :test-id i
                                    :batch "async-test"))
          
          ;; 批量异步写入测试
          (dotimes (i (/ count 2))
            (elilog--log-with-logger batch-logger 'info 
                                    (format "批量异步性能测试日志 #%d" i)
                                    :test-id i
                                    :batch "batch-test"))
          
          (let ((async-time (float-time (time-subtract (current-time) start-time))))
            (message "异步模式：%d条日志，提交耗时 %.3f秒，平均 %.3f条/秒" 
                     count async-time (/ count async-time)))
          
          ;; 等待异步处理完成并显示统计
          (when (fboundp 'elilog-async-statistics)
            (run-with-timer 3 nil (lambda ()
                                   (message "异步处理统计: %s" 
                                           (elilog-async-statistics))
                                   (message "性能测试完成！检查文件:")
                                   (message "- /tmp/perf-sync.log (同步)")
                                   (message "- /tmp/perf-async.log (异步)")
                                   (message "- /tmp/perf-batch.log (批量异步)"))))))))

;;; ===== 运行所有示例 =====

(defun elilog-run-all-examples ()
  "运行所有示例。"
  (interactive)
  (message "\n========== Elilog 框架示例演示 ==========\n")
  
  ;; 初始化框架
  (elilog-initialize)
  
  ;; 运行各种示例
  (elilog-example-basic-usage)
  (message "")
  
  (elilog-example-multiple-sinks)
  (message "")
  
  (elilog-example-context-management)
  (message "")
  
  (elilog-example-filters)
  (message "")
  
  (elilog-example-custom-formatter)
  (message "")
  
  ;; 异步功能演示
  (elilog-example-async-logging)
  (message "")
  
  (elilog-example-advanced-async)
  (message "")
  
  ;; 性能测试（可选）
  ;; (elilog-performance-test)
  
  (message "\n========== 示例演示完成 ==========\n")
  (message "检查以下文件查看日志输出：")
  (message "- /tmp/elilog-test.log")
  (message "- /tmp/elilog-test.json") 
  (message "- Buffer: *elilog-test*")
  (message "")
  (message "异步功能输出文件：")
  (message "- /tmp/elilog-async-new.log (异步文件)")
  (message "- /tmp/elilog-batch-new.log (批量异步)")
  (message "- /tmp/elilog-smart-new.log (智能异步)")
  (message "- /tmp/high-perf.log (高性能批量)")
  (message "")
  (message "要运行性能测试，请执行: (elilog-performance-test)")
  (message "异步模块状态: %s" 
           (if (elilog-sinks-async-available-p) "可用" "不可用")))

;;; ===== 配置示例 =====

(defun elilog-example-configuration ()
  "配置示例 - 展示如何在实际项目中配置日志框架。"
  (interactive)
  (message "=== 实际项目配置示例 ===")
  
  ;; 为不同组件创建专门的记录器
  
  ;; Web服务器日志
  (elilog-configure-logger "web-server"
                          :level 'info
                          :sinks (list 
                                  (elilog-sinks-create-file "/tmp/web-server.log")
                                  (elilog-sinks-create-console))
                          :context '(:component "web-server" :port 8080))
  
  ;; 数据库日志
  (elilog-configure-logger "database" 
                          :level 'warning
                          :sinks (list
                                  (elilog-sinks-create-file "/tmp/database.log")
                                  (elilog-sinks-create-buffer "*database-logs*"))
                          :context '(:component "database" :host "localhost"))
  
  ;; API日志 - 使用JSON格式
  (elilog-configure-logger "api"
                          :level 'debug  
                          :sinks (list
                                  (elilog-sinks-create-file "/tmp/api.json"
                                                           (elilog-formatters-create-json)))
                          :context '(:component "api" :version "v1"))
  
  ;; 测试各个记录器
  (let ((web-logger (elilog-get-logger "web-server"))
        (db-logger (elilog-get-logger "database"))
        (api-logger (elilog-get-logger "api")))
    
    (elilog--log-with-logger web-logger 'info "Web服务器启动" 
                            :bind-address "0.0.0.0")
    (elilog--log-with-logger web-logger 'info "处理HTTP请求" 
                            :method "GET" :path "/api/users" :status 200)
    
    (elilog--log-with-logger db-logger 'warning "数据库连接池接近满载" 
                            :pool-size 95 :max-size 100)
    (elilog--log-with-logger db-logger 'error "查询超时" 
                            :query "SELECT * FROM users" :timeout 30)
    
    (elilog--log-with-logger api-logger 'debug "API调用开始" 
                            :endpoint "/users" :user-id 123)
    (elilog--log-with-logger api-logger 'info "API调用完成" 
                            :endpoint "/users" :duration-ms 45 :result-count 25))
  
  (message "配置示例完成！检查以下文件：")
  (message "- /tmp/web-server.log")
  (message "- /tmp/database.log") 
  (message "- /tmp/api.json")
  (message "- Buffer: *database-logs*")))

(provide 'elilog-examples)

;;; elilog-examples.el ends here
