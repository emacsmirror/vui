;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "vui.el"
  (it "loads successfully"
    (expect (featurep 'vui) :to-be-truthy)))

;;; vui-test.el ends here
