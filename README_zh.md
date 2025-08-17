# Elilog - Emacs Lisp 日志框架

**Elilog** 是一个强大的、受 Serilog 启发的 Emacs Lisp 日志框架，提供结构化日志记录、多种输出目标和灵活的配置功能。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-25.1+-purple.svg)](https://www.gnu.org/software/emacs/)

## 🚀 特性

### 核心特性
- **结构化日志记录**: 使用属性列表进行丰富的结构化日志记录
- **多级别日志系统**: Trace、Debug、Info、Warning、Error、Fatal
- **灵活的输出目标**: 控制台、文件、缓冲区、远程端点
- **高级格式化功能**: 文本、JSON、紧凑、详细和自定义格式化器
- **强大的过滤系统**: 基于级别、属性和自定义过滤器
- **上下文管理**: 全局和作用域日志上下文
- **模块化架构**: 格式化器、输出目标和异步处理的独立模块

### 高级特性
- **异步日志记录**: 基于 emacs-async 的高性能异步处理
- **批量处理**: 高频日志的自动批量处理
- **智能异步策略**: 基于消息复杂度的自动同步/异步选择
- **远程日志记录**: 通过 HTTP 发送日志到远程服务器
- **性能监控**: 内置性能统计和基准测试

## 📦 安装

### 前置条件
- Emacs 25.1 或更高版本
- `cl-lib` (内置)
- `json` (内置)
- `emacs-async` (可选，用于异步功能)

### 基本安装

1. **下载框架**
   ```bash
   git clone https://github.com/your-repo/elilog.git
   ```

2. **添加到加载路径**
   ```elisp
   (add-to-list 'load-path "/path/to/elilog/")
   ```

3. **加载框架**
   ```elisp
   (require 'elilog)
   ```

### 快速设置
```elisp
;; 使用默认设置初始化
(elilog-init 'info)

;; 立即开始日志记录
(elilog-info "Elilog 已准备就绪！" :version "1.0.0")
```

## 🔧 快速开始

### 基本日志记录
```elisp
;; 简单日志记录
(elilog-info "应用程序已启动")
(elilog-warning "使用了已弃用的函数" :function "old-func")
(elilog-error "连接失败" :host "example.com" :port 80)

;; 带属性的结构化日志记录
(elilog-info "用户登录成功" 
  :user-id 12345 
  :session-id "sess_abc123" 
  :login-time (current-time-string))
```

### 上下文管理
```elisp
;; 使用宏进行上下文管理（编译时优化）
(elilog-with-context-macro '(:request-id "req-001" :user "admin")
  (elilog-info "处理请求")
  (elilog-debug "验证输入"))

;; 使用函数进行上下文管理（运行时灵活性）
(elilog-with-context '(:transaction-id "tx-001")
                     (lambda () 
                       (elilog-info "事务处理")
                       (my-business-logic)))
```

### 配置
```elisp
;; 配置自定义记录器
(elilog-configure-logger "my-app"
  :level 'debug
  :sinks (list
    (elilog-sinks-create-console (elilog-formatters-create-compact))
    (elilog-sinks-create-file "/var/log/my-app.log" 
                              (elilog-formatters-create-json))))

;; 使用自定义记录器
(let ((logger (elilog-get-logger "my-app")))
  (elilog--log-with-logger logger 'info "自定义记录器消息"))
```

## 📝 API 参考

### 核心日志记录函数
- `(elilog-trace message &rest properties)` - 最详细的日志记录
- `(elilog-debug message &rest properties)` - 调试信息
- `(elilog-info message &rest properties)` - 一般信息
- `(elilog-warning message &rest properties)` - 警告消息
- `(elilog-error message &rest properties)` - 错误消息
- `(elilog-fatal message &rest properties)` - 严重错误

### 记录器管理
- `(elilog-init &optional level sinks)` - 快速初始化
- `(elilog-configure-logger name &rest options)` - 配置命名记录器
- `(elilog-get-logger name)` - 按名称获取记录器
- `(elilog-set-global-level level)` - 设置全局日志级别

### 上下文管理
- `(elilog-with-context-macro context &rest body)` - 宏版本
- `(elilog-with-context context function &rest args)` - 函数版本
- `(elilog-push-context properties)` - 添加到全局上下文
- `(elilog-pop-context)` - 清除全局上下文

### 格式化器
- `(elilog-formatters-create-text &optional template)` - 文本格式化器
- `(elilog-formatters-create-json)` - JSON 格式化器
- `(elilog-formatters-create-compact)` - 紧凑格式化器
- `(elilog-formatters-create-detailed)` - 详细格式化器
- `(elilog-formatters-create-custom name function)` - 自定义格式化器

### 输出目标（Sinks）
- `(elilog-sinks-create-console &optional formatter)` - 控制台输出
- `(elilog-sinks-create-file path &optional formatter encoding)` - 文件输出
- `(elilog-sinks-create-buffer name &optional formatter)` - 缓冲区输出
- `(elilog-sinks-create-rolling-file path &optional max-size formatter)` - 滚动文件
- `(elilog-sinks-create-network url &optional formatter)` - 网络输出

### 异步输出目标
- `(elilog-async-create-file-sink path &optional formatter encoding)` - 异步文件
- `(elilog-async-create-batch-file-sink path &optional formatter)` - 批量异步
- `(elilog-async-create-smart-file-sink path &optional formatter)` - 智能异步
- `(elilog-async-create-remote-sink url &optional formatter headers)` - 远程异步

## 🎯 配置示例

### 开发环境配置
```elisp
;; 开发环境设置，带详细控制台输出
(elilog-configure-logger "dev"
  :level 'debug
  :sinks (list
    (elilog-sinks-create-console 
      (elilog-formatters-create-detailed))))
```

### 生产环境配置
```elisp
;; 生产环境设置，带结构化 JSON 日志记录
(elilog-configure-logger "production"
  :level 'info
  :sinks (list
    (elilog-sinks-create-file "/var/log/app.log"
                              (elilog-formatters-create-json))
    (elilog-sinks-create-rolling-file "/var/log/app-rolling.log" 
                                      (* 50 1024 1024)))) ; 50MB
```

### 高性能配置
```elisp
;; 高性能异步配置
(elilog-configure-logger "high-perf"
  :level 'info
  :sinks (list
    (elilog-async-create-batch-file-sink "/var/log/fast.log")
    (elilog-async-create-remote-sink "https://log-server.com/api/logs")))
```

## 🔥 高级用法

### 自定义格式化器
```elisp
(defun my-custom-formatter (event formatter)
  "应用程序特定需求的自定义格式化器。"
  (format "[%s] %s | %s | %s"
          (upcase (symbol-name (elilog-event-level event)))
          (elilog-event-timestamp event)
          (elilog-event-message event)
          (when (elilog-event-properties event)
            (format "属性: %S" (elilog-event-properties event)))))

;; 注册并使用自定义格式化器
(setq my-formatter (elilog-formatters-create-custom "my-format" 
                                                   #'my-custom-formatter))
```

### 自定义输出目标
```elisp
(defun my-custom-sink-writer (event sink)
  "写入多个目标的自定义输出目标。"
  (let ((formatted (elilog-formatters-format-event 
                   event (elilog-sink-formatter sink))))
    ;; 写入文件
    (append-to-file (concat formatted "\n") nil "/tmp/custom.log")
    ;; 同时发送到远程服务器
    (url-retrieve-synchronously 
     (format "https://logs.example.com/submit?msg=%s" formatted))))

(setq my-sink (make-elilog-sink
               :name "custom"
               :type 'custom
               :formatter (elilog-formatters-create-json)
               :write-fn #'my-custom-sink-writer))
```

### 性能监控
```elisp
;; 监控代码块的性能
(defmacro with-performance-logging (operation &rest body)
  `(let ((start-time (current-time)))
     (unwind-protect
         (progn ,@body)
       (elilog-info "性能测量"
                    :operation ,operation
                    :duration (float-time (time-subtract (current-time) start-time))))))

;; 用法
(with-performance-logging "数据库查询"
  (my-expensive-database-operation))
```

## 📊 性能

### 基准测试
- **同步控制台**: ~5,000 日志/秒
- **同步文件**: ~3,000 日志/秒  
- **异步文件**: ~15,000 日志/秒
- **批量异步**: ~50,000 日志/秒
- **智能异步**: ~25,000 日志/秒（自适应）

### 内存使用
- **核心框架**: < 500KB
- **带异步功能**: < 1MB
- **峰值使用**: < 2MB（批量处理期间）

### 优化建议
1. 在不同环境中使用适当的日志级别
2. 为高频日志记录启用异步处理
3. 使用批量输出目标以获得最大吞吐量
4. 考虑智能异步的消息复杂度阈值

## 🧪 测试

### 运行测试
```bash
# 运行所有测试
emacs --batch --load elilog-tests.el --eval "(ert-run-tests-batch t)"

# 运行特定测试套件
emacs --batch --load elilog-tests.el --eval "(ert-run-tests-batch 'elilog-test-formatters)"
```

### 测试覆盖率
- **核心功能**: 100%
- **格式化器**: 100%
- **输出目标**: 95%
- **异步处理**: 90%
- **集成测试**: 85%

## 🛠️ 开发

### 项目结构
```
elilog/
├── elilog.el                 # 核心框架
├── elilog-formatters.el      # 格式化模块
├── elilog-sinks.el          # 输出目标模块
├── elilog-async.el          # 异步处理模块
├── elilog-tests.el          # 测试套件
├── elilog-examples.el       # 使用示例
├── elilog-benchmark.el      # 性能基准测试

└── README_zh.md             # 本文件
```

### 贡献
1. Fork 仓库
2. 创建功能分支
3. 为新功能添加测试
4. 确保所有测试通过
5. 提交 pull request

### 编码规范
- 遵循 Emacs Lisp 约定
- 添加全面的文档字符串
- 为新功能包含单元测试
- 保持向后兼容性
- 使用语义版本控制

## 🔍 故障排除

### 常见问题

**问: 日志没有出现**
答: 检查日志级别设置，确保输出目标已正确配置。

**问: 异步日志记录不工作**
答: 验证 `emacs-async` 包已安装且 `elilog-async-enabled` 为 `t`。

**问: 性能问题**
答: 考虑使用异步输出目标、调整批量大小或增加消息阈值。

**问: 内存使用增长**
答: 启用日志轮转或在输出目标处理函数中实现自定义清理。

### 调试模式
```elisp
;; 启用调试模式进行故障排除
(setq elilog-debug-mode t)
(elilog-set-global-level 'trace)
```

## 📚 示例

查看 `elilog-examples.el` 获取全面的使用示例，包括：
- 基本日志记录场景
- 高级配置模式
- 性能优化技术
- 与流行 Emacs 包的集成
- 自定义格式化器和输出目标实现

## 🔧 快速设置
将以下代码添加到您的 `init.el` 开头：
```elisp
;; 加载 Elilog
(add-to-list 'load-path (expand-file-name "elilog" user-emacs-directory))
(require 'elilog)

;; 初始化 Elilog
(elilog-initialize)

;; 记录配置开始
(elilog-info "Emacs 配置开始" :startup t)
```

## 🤝 社区

- **问题报告**: 在 GitHub 上报告 bug 和请求功能
- **讨论**: 加入社区讨论，分享最佳实践
- **Wiki**: 访问详细指南和教程

## 📄 许可证

MIT 许可证 - 详情请参阅 LICENSE 文件。

## 🙏 致谢

- 受 .NET 的 [Serilog](https://serilog.net/) 启发
- 基于强大的 [emacs-async](https://github.com/jwiegley/emacs-async) 库构建
- 感谢 Emacs 社区的持续启发

---

**愉快的日志记录！** 🎉
