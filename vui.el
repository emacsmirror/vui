;;; vui.el --- Declarative, component-based UI library for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Boris

;; Author: Boris
;; URL: https://github.com/d12frosted/vui.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ui, widgets, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; vui.el (Virtual UI) is a declarative, component-based UI library for Emacs.
;; It brings React-like patterns to buffer-based interfaces: define components
;; as functions of state, compose them into trees, and let the library handle
;; efficient updates through reconciliation.
;;
;; Built on top of `widget.el' for battle-tested input handling, vui.el adds
;; state management, automatic re-rendering, cursor preservation, and layout
;; primitives for building complex, interactive UIs in Emacs buffers.
;;
;; Core Philosophy:
;; - Declarative: Describe the UI as a function of state
;; - Component-based: Build complex UIs from small, reusable pieces
;; - Unidirectional data flow: Data flows down through props; events flow up
;; - Predictable: Same props + state always produces the same output
;; - Emacs-native: Respect Emacs conventions (point, markers, keymaps, faces)

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)

;;; Custom Variables

(defgroup vui nil
  "Declarative, component-based UI library for Emacs."
  :group 'tools
  :prefix "vui-")

;;; Core Data Structures

;; TODO: Implement core data structures from design doc

;;; Public API

;; TODO: Implement public API

(provide 'vui)
;;; vui.el ends here
