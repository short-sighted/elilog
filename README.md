# Elilog - Emacs Lisp Logging Framework

A powerful, Serilog-inspired logging framework for Emacs Lisp.

一个强大的、受 Serilog 启发的 Emacs Lisp 日志框架。

## Documentation / 文档

- **English**: [README_en.md](README_en.md)
- **中文**: [README_zh.md](README_zh.md)

## Quick Start / 快速开始

```elisp
;; Load the framework / 加载框架
(require 'elilog)

;; Initialize with default settings / 使用默认设置初始化
(elilog-init 'info)

;; Start logging / 开始记录日志
(elilog-info "Hello, Elilog!" :version "1.0.0")
```

## License / 许可证

MIT License - see LICENSE file for details.

MIT 许可证 - 详情请参阅 LICENSE 文件。
