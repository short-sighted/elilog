;;; elilog-benchmark.el --- Elilog performance benchmarking suite  -*- lexical-binding: t; -*-

;; Author: Elilog Development Team
;; Maintainer: Elilog Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: logging, benchmark, performance, testing
;; URL: https://github.com/elilog/elilog

;;; Commentary:
;; This file contains performance benchmarks for the Elilog logging framework.
;; It compares performance across synchronous, asynchronous, and batch async modes.
;;
;; 这个文件包含了Elilog日志框架的性能基准测试，
;; 用于对比同步、异步、批量异步等不同模式的性能。
;;
;; Key Features / 主要特性:
;; - Comprehensive performance testing / 全面的性能测试
;; - Multiple logging scenarios / 多种日志记录场景
;; - Async vs sync comparison / 异步与同步对比
;; - Memory usage monitoring / 内存使用监控
;; - Detailed performance reports / 详细的性能报告
;;
;; Usage Examples / 使用示例:
;; (elilog-benchmark-run-all)
;; (elilog-benchmark-compare-modes)
;; (elilog-benchmark-memory-usage)

;;; Code:

(require 'cl-lib)
(require 'elilog)

;; Try to load async module / 尝试加载异步模块
(defvar elilog-benchmark-async-available-p
  (condition-case nil
      (progn (require 'elilog-async) t)
    (error nil))
  "Whether async module is available for benchmarking.
异步模块是否可用于基准测试。")

;;; ===== 基准测试配置 =====

(defvar elilog-benchmark-test-counts '(100 500 1000 2000)
  "基准测试的日志数量列表。")

(defvar elilog-benchmark-message-sizes '(short medium long)
  "测试消息的不同长度类型。")

(defvar elilog-benchmark-results nil
  "基准测试结果存储。")

;;; ===== 测试消息生成 =====

(defun elilog-benchmark--generate-message (size index)
  "生成指定大小的测试消息。"
  (pcase size
    ('short (format "Short log #%d" index))
    ('medium (format "Medium log message #%d with some additional context and details that make it longer than short messages" index))
    ('long (format "Long log message #%d with extensive details, context information, error descriptions, stack traces, user data, session information, request parameters, response data, timing information, and other relevant metadata that would typically be found in production log entries. This message is intentionally verbose to test performance with larger log entries that contain substantial amounts of information." index))))

(defun elilog-benchmark--generate-properties (size index)
  "生成指定复杂度的属性。"
  (pcase size
    ('short `(:id ,index))
    ('medium `(:id ,index :timestamp ,(current-time-string) :type "test"))
    ('long `(:id ,index 
             :timestamp ,(current-time-string)
             :type "benchmark"
             :session-id ,(format "session-%d" (random 10000))
             :user-id ,(+ 1000 (random 9000))
             :action "test-action"
             :duration ,(random 1000)
             :result "success"
             :metadata (:version "1.0" :build "123" :env "test")
             :tags ("performance" "testing" "benchmark")))))

;;; ===== 基准测试函数 =====

(defun elilog-benchmark-sync (count message-size)
  "同步模式基准测试。"
  (let* ((test-file (format "/tmp/elilog-benchmark-sync-%s-%d.log" message-size count))
         (sink (elilog-sinks-create-file test-file))
         (start-time (current-time)))
    
    (elilog-configure-logger "benchmark-sync"
                            :level 'debug
                            :sinks (list sink))
    
    (let ((logger (elilog-get-logger "benchmark-sync")))
      (dotimes (i count)
        (elilog--log-with-logger logger 'info 
                                (elilog-benchmark--generate-message message-size i)
                                (elilog-benchmark--generate-properties message-size i))))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; 清理测试文件
      (when (file-exists-p test-file)
        (delete-file test-file))
      elapsed)))

(defun elilog-benchmark-async-single (count message-size)
  "异步单条模式基准测试。"
  (if (not elilog-benchmark-async-available-p)
      nil
    (let* ((test-file (format "/tmp/elilog-benchmark-async-%s-%d.log" message-size count))
           (sink (elilog-sinks-create-async-file test-file))
           (start-time (current-time)))
      
      (elilog-configure-logger "benchmark-async"
                              :level 'debug
                              :sinks (list sink))
      
      (let ((logger (elilog-get-logger "benchmark-async")))
        (dotimes (i count)
          (elilog--log-with-logger logger 'info 
                                  (elilog-benchmark--generate-message message-size i)
                                  (elilog-benchmark--generate-properties message-size i))))
      
      (let ((submit-time (float-time (time-subtract (current-time) start-time))))
        ;; 等待异步处理完成
        (sleep-for 3)
        ;; 清理测试文件
        (when (file-exists-p test-file)
          (delete-file test-file))
        submit-time))))

(defun elilog-benchmark-async-batch (count message-size)
  "异步批量模式基准测试。"
  (if (not elilog-benchmark-async-available-p)
      nil
    (let* ((test-file (format "/tmp/elilog-benchmark-batch-%s-%d.log" message-size count))
           (sink (elilog-sinks-create-batch-file test-file))
           (start-time (current-time)))
      
      (elilog-configure-logger "benchmark-batch"
                              :level 'debug
                              :sinks (list sink))
      
      (let ((logger (elilog-get-logger "benchmark-batch")))
        (dotimes (i count)
          (elilog--log-with-logger logger 'info 
                                  (elilog-benchmark--generate-message message-size i)
                                  (elilog-benchmark--generate-properties message-size i))))
      
      (let ((submit-time (float-time (time-subtract (current-time) start-time))))
        ;; 手动刷新批量队列
        (when (fboundp 'elilog-async-flush-all-batches)
          (elilog-async-flush-all-batches))
        ;; 等待批量处理完成
        (sleep-for 3)
        ;; 清理测试文件
        (when (file-exists-p test-file)
          (delete-file test-file))
        submit-time))))

(defun elilog-benchmark-null-sink (count message-size)
  "空Sink基准测试（测试纯处理开销）。"
  (let* ((sink (elilog-sinks-create-null))
         (start-time (current-time)))
    
    (elilog-configure-logger "benchmark-null"
                            :level 'debug
                            :sinks (list sink))
    
    (let ((logger (elilog-get-logger "benchmark-null")))
      (dotimes (i count)
        (elilog--log-with-logger logger 'info 
                                (elilog-benchmark--generate-message message-size i)
                                (elilog-benchmark--generate-properties message-size i))))
    
    (float-time (time-subtract (current-time) start-time))))

;;; ===== 综合基准测试 =====

(defun elilog-run-benchmark-suite ()
  "运行完整的基准测试套件。"
  (interactive)
  (message "开始Elilog性能基准测试...")
  (message "异步模块状态: %s" 
           (if elilog-benchmark-async-available-p "可用" "不可用"))
  
  (setq elilog-benchmark-results nil)
  
  ;; 预热
  (message "预热中...")
  (elilog-benchmark-sync 50 'short)
  (when elilog-benchmark-async-available-p
    (elilog-benchmark-async-single 50 'short))
  
  (message "开始正式测试...")
  
  ;; 遍历所有测试组合
  (dolist (count elilog-benchmark-test-counts)
    (dolist (msg-size elilog-benchmark-message-sizes)
      (message "测试 %d 条 %s 消息..." count msg-size)
      
      ;; 空Sink测试（基准）
      (let ((null-time (elilog-benchmark-null-sink count msg-size)))
        (push `(:type null :count ,count :size ,msg-size :time ,null-time :rate ,(/ count null-time))
              elilog-benchmark-results))
      
      ;; 同步测试
      (let ((sync-time (elilog-benchmark-sync count msg-size)))
        (push `(:type sync :count ,count :size ,msg-size :time ,sync-time :rate ,(/ count sync-time))
              elilog-benchmark-results))
      
      ;; 异步测试
      (when elilog-benchmark-async-available-p
        (let ((async-time (elilog-benchmark-async-single count msg-size)))
          (when async-time
            (push `(:type async :count ,count :size ,msg-size :time ,async-time :rate ,(/ count async-time))
                  elilog-benchmark-results)))
        
        ;; 批量异步测试
        (let ((batch-time (elilog-benchmark-async-batch count msg-size)))
          (when batch-time
            (push `(:type batch :count ,count :size ,msg-size :time ,batch-time :rate ,(/ count batch-time))
                  elilog-benchmark-results))))
      
      ;; 等待异步操作完成
      (sleep-for 1)))
  
  (message "基准测试完成！")
  (elilog-benchmark-display-results))

(defun elilog-benchmark-display-results ()
  "显示基准测试结果。"
  (interactive)
  (when (not elilog-benchmark-results)
    (message "没有可用的基准测试结果，请先运行 (elilog-run-benchmark-suite)")
    (return))
  
  (with-current-buffer (get-buffer-create "*Elilog Benchmark Results*")
    (erase-buffer)
    (insert "=== Elilog 性能基准测试结果 ===\n\n")
    
    ;; 按消息大小分组显示结果
    (dolist (msg-size elilog-benchmark-message-sizes)
      (insert (format "=== %s 消息测试结果 ===\n" (upcase (symbol-name msg-size))))
      (insert (format "%-8s %-8s %-8s %-12s %-12s\n" "类型" "数量" "耗时(s)" "速率(条/s)" "相对性能"))
      (insert (make-string 60 ?-) "\n")
      
      ;; 获取该消息大小的所有结果
      (let ((size-results (cl-remove-if-not 
                          (lambda (r) (eq (plist-get r :size) msg-size))
                          (reverse elilog-benchmark-results))))
        
        ;; 按数量分组
        (dolist (count elilog-benchmark-test-counts)
          (let ((count-results (cl-remove-if-not 
                               (lambda (r) (eq (plist-get r :count) count))
                               size-results)))
            
            (when count-results
              (insert (format "\n--- %d 条日志 ---\n" count))
              
              ;; 找到基准性能（null sink）
              (let* ((null-result (cl-find-if (lambda (r) (eq (plist-get r :type) 'null)) count-results))
                     (baseline-rate (if null-result (plist-get null-result :rate) 1)))
                
                (dolist (result count-results)
                  (let ((type (plist-get result :type))
                        (time (plist-get result :time))
                        (rate (plist-get result :rate))
                        (relative (/ (plist-get result :rate) baseline-rate)))
                    (insert (format "%-8s %-8d %-8.3f %-12.0f %-12.2fx\n"
                                  (symbol-name type) count time rate relative))))))))
      
      (insert "\n"))
    
    ;; 显示统计摘要
    (insert "=== 性能摘要 ===\n")
    (when elilog-benchmark-async-available-p
      (let ((sync-rates (mapcar (lambda (r) (plist-get r :rate))
                               (cl-remove-if-not (lambda (r) (eq (plist-get r :type) 'sync))
                                                 elilog-benchmark-results)))
            (async-rates (mapcar (lambda (r) (plist-get r :rate))
                                (cl-remove-if-not (lambda (r) (eq (plist-get r :type) 'async))
                                                  elilog-benchmark-results)))
            (batch-rates (mapcar (lambda (r) (plist-get r :rate))
                                (cl-remove-if-not (lambda (r) (eq (plist-get r :type) 'batch))
                                                  elilog-benchmark-results))))
        
        (when sync-rates
          (insert (format "同步模式平均速率: %.0f 条/秒\n" 
                         (/ (apply '+ sync-rates) (length sync-rates)))))
        (when async-rates  
          (insert (format "异步模式平均速率: %.0f 条/秒\n"
                         (/ (apply '+ async-rates) (length async-rates)))))
        (when batch-rates
          (insert (format "批量异步模式平均速率: %.0f 条/秒\n"
                         (/ (apply '+ batch-rates) (length batch-rates)))))
        
        (when (and async-rates sync-rates)
          (let ((improvement (/ (/ (apply '+ async-rates) (length async-rates))
                               (/ (apply '+ sync-rates) (length sync-rates)))))
            (insert (format "异步相对同步性能提升: %.2fx\n" improvement))))))
    
    (insert "\n异步模块状态: ")
    (if elilog-benchmark-async-available-p
        (insert "可用 (基于emacs-async)")
      (insert "不可用 (需要安装emacs-async包)"))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; ===== 快速性能测试 =====

(defun elilog-quick-benchmark ()
  "快速性能测试。"
  (interactive)
  (message "=== 快速性能测试 ===")
  
  (let ((count 500))
    ;; 同步测试
    (let ((sync-time (elilog-benchmark-sync count 'medium)))
      (message "同步模式: %d条日志, %.3f秒, %.0f条/秒" 
               count sync-time (/ count sync-time)))
    
    ;; 异步测试
    (when elilog-benchmark-async-available-p
      (let ((async-time (elilog-benchmark-async-single count 'medium)))
        (when async-time
          (message "异步模式: %d条日志, %.3f秒提交时间, %.0f条/秒" 
                   count async-time (/ count async-time))))
      
      (let ((batch-time (elilog-benchmark-async-batch count 'medium)))
        (when batch-time
          (message "批量异步: %d条日志, %.3f秒提交时间, %.0f条/秒" 
                   count batch-time (/ count batch-time))))
      
      ;; 显示异步统计
      (when (fboundp 'elilog-async-statistics)
        (run-with-timer 2 nil (lambda ()
                               (message "异步统计: %s" (elilog-async-statistics))))))
    
    (message "快速性能测试完成！")))

;;; ===== 内存使用测试 =====

(defun elilog-memory-benchmark ()
  "内存使用基准测试。"
  (interactive)
  (message "=== 内存使用基准测试 ===")
  
  (let ((initial-memory (memory-use-counts)))
    (message "初始内存使用: %s" initial-memory)
    
    ;; 大量日志测试
    (let ((count 5000))
      (elilog-configure-logger "memory-test"
                              :level 'debug
                              :sinks (list (elilog-sinks-create-null)))
      
      (let ((logger (elilog-get-logger "memory-test")))
        (dotimes (i count)
          (elilog--log-with-logger logger 'info 
                                  (format "Memory test log #%d with some data %s" 
                                         i (make-string 50 ?x))
                                  :id i :data (make-string 100 ?y))))
      
      (garbage-collect)
      (let ((final-memory (memory-use-counts)))
        (message "处理%d条日志后内存使用: %s" count final-memory)
        (message "内存增长: cons cells: %d, symbols: %d" 
                 (- (nth 0 final-memory) (nth 0 initial-memory))
                 (- (nth 1 final-memory) (nth 1 initial-memory))))))))

;;; ===== Main Runner Functions / 主要运行函数 =====

(defun elilog-benchmark-run-all ()
  "Run all benchmark tests.
运行所有基准测试。

This function executes all available benchmarks and displays results.
此函数执行所有可用的基准测试并显示结果。"
  (interactive)
  (message "=== Elilog Complete Benchmark Suite ===")
  (let ((start-time (current-time)))
    
    ;; Run synchronous benchmarks / 运行同步基准测试
    (message "\n--- Synchronous Benchmarks ---")
    (elilog-benchmark-sync-file)
    (elilog-benchmark-sync-console)
    
    ;; Run async benchmarks if available / 如果可用则运行异步基准测试
    (when elilog-benchmark-async-available-p
      (message "\n--- Asynchronous Benchmarks ---")
      (elilog-benchmark-async-file)
      (elilog-benchmark-batch-async))
    
    ;; Run memory benchmark / 运行内存基准测试
    (message "\n--- Memory Usage Benchmark ---")
    (elilog-memory-benchmark)
    
    ;; Show summary / 显示摘要
    (let ((total-time (float-time (time-subtract (current-time) start-time))))
      (message "\n=== Benchmark Summary ===")
      (message "Total benchmark time: %.3f seconds" total-time)
      (message "Benchmark suite completed successfully!"))))

(provide 'elilog-benchmark)

;;; elilog-benchmark.el ends here
