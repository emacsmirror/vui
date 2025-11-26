;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "vui.el"
  (it "loads successfully"
    (expect (featurep 'vui) :to-be-truthy)))

(describe "vui-text"
  (it "creates a text vnode"
    (let ((node (vui-text "hello")))
      (expect (vui-vnode-text-p node) :to-be-truthy)
      (expect (vui-vnode-text-content node) :to-equal "hello")))

  (it "accepts face property"
    (let ((node (vui-text "hello" :face 'bold)))
      (expect (vui-vnode-text-face node) :to-equal 'bold)))

  (it "accepts key property"
    (let ((node (vui-text "hello" :key "my-key")))
      (expect (vui-vnode-key node) :to-equal "my-key"))))

(describe "vui-fragment"
  (it "creates a fragment with children"
    (let ((node (vui-fragment (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-fragment-p node) :to-be-truthy)
      (expect (length (vui-vnode-fragment-children node)) :to-equal 2))))

(describe "vui-newline"
  (it "creates a newline vnode"
    (let ((node (vui-newline)))
      (expect (vui-vnode-newline-p node) :to-be-truthy))))

(describe "vui-space"
  (it "creates a space vnode with default width"
    (let ((node (vui-space)))
      (expect (vui-vnode-space-p node) :to-be-truthy)
      (expect (vui-vnode-space-width node) :to-equal 1)))

  (it "accepts custom width"
    (let ((node (vui-space 5)))
      (expect (vui-vnode-space-width node) :to-equal 5))))

(describe "vui-button"
  (it "creates a button vnode"
    (let ((node (vui-button "Click me")))
      (expect (vui-vnode-button-p node) :to-be-truthy)
      (expect (vui-vnode-button-label node) :to-equal "Click me")))

  (it "accepts on-click callback"
    (let* ((clicked nil)
           (node (vui-button "Click" :on-click (lambda () (setq clicked t)))))
      (expect (vui-vnode-button-on-click node) :to-be-truthy)))

  (it "accepts disabled property"
    (let ((node (vui-button "Click" :disabled t)))
      (expect (vui-vnode-button-disabled-p node) :to-be-truthy)))

  (it "accepts key property"
    (let ((node (vui-button "Click" :key "btn-1")))
      (expect (vui-vnode-key node) :to-equal "btn-1"))))

(describe "vui-render"
  (it "renders text to buffer"
    (with-temp-buffer
      (vui-render (vui-text "hello"))
      (expect (buffer-string) :to-equal "hello")))

  (it "renders text with face"
    (with-temp-buffer
      (vui-render (vui-text "hello" :face 'bold))
      (expect (buffer-string) :to-equal "hello")
      (expect (get-text-property 0 'face (buffer-string)) :to-equal 'bold)))

  (it "renders fragment children in order"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "abc")))

  (it "renders newlines"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "line1") (vui-newline) (vui-text "line2")))
      (expect (buffer-string) :to-equal "line1\nline2")))

  (it "renders spaces"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") (vui-space 3) (vui-text "b")))
      (expect (buffer-string) :to-equal "a   b")))

  (it "handles nested fragments"
    (with-temp-buffer
      (vui-render (vui-fragment
                   (vui-text "start")
                   (vui-fragment (vui-text "-") (vui-text "nested") (vui-text "-"))
                   (vui-text "end")))
      (expect (buffer-string) :to-equal "start-nested-end")))

  (it "skips nil children"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") nil (vui-text "b")))
      (expect (buffer-string) :to-equal "ab")))

  (it "handles plain strings as shorthand"
    (with-temp-buffer
      (vui-render (vui-fragment "hello" " " "world"))
      (expect (buffer-string) :to-equal "hello world")))

  (it "renders button with label"
    (with-temp-buffer
      (vui-render (vui-button "Click me"))
      (expect (buffer-string) :to-match "Click me")))

  (it "renders disabled button as text"
    (with-temp-buffer
      (vui-render (vui-button "Disabled" :disabled t))
      (expect (buffer-string) :to-equal "[Disabled]")))

  (it "triggers on-click callback when button is activated"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-button "Click" :on-click (lambda () (setq clicked t))))
        ;; Find and activate the widget
        (goto-char (point-min))
        (widget-forward 1)
        (widget-button-press (point))
        (expect clicked :to-be-truthy)))))

;;; vui-test.el ends here
