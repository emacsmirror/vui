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

(describe "vui-field"
  (it "creates a field vnode"
    (let ((node (vui-field "hello")))
      (expect (vui-vnode-field-p node) :to-be-truthy)
      (expect (vui-vnode-field-value node) :to-equal "hello")))

  (it "defaults to empty string"
    (let ((node (vui-field nil)))
      (expect (vui-vnode-field-value node) :to-equal "")))

  (it "accepts size property"
    (let ((node (vui-field "" :size 30)))
      (expect (vui-vnode-field-size node) :to-equal 30)))

  (it "accepts placeholder property"
    (let ((node (vui-field "" :placeholder "Enter text...")))
      (expect (vui-vnode-field-placeholder node) :to-equal "Enter text...")))

  (it "accepts on-change callback"
    (let ((node (vui-field "" :on-change #'identity)))
      (expect (vui-vnode-field-on-change node) :to-be-truthy)))

  (it "accepts key property"
    (let ((node (vui-field "" :key "field-1")))
      (expect (vui-vnode-key node) :to-equal "field-1"))))

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
        (expect clicked :to-be-truthy))))

  (it "renders field with value"
    (with-temp-buffer
      (vui-render (vui-field "hello"))
      (expect (buffer-string) :to-match "hello")))

  (it "renders field with placeholder when empty"
    (with-temp-buffer
      (vui-render (vui-field "" :placeholder "Enter name"))
      (expect (buffer-string) :to-match "Enter name")))

  (it "triggers on-change callback when field is modified"
    (with-temp-buffer
      (let ((new-value nil))
        (vui-render (vui-field "initial" :on-change (lambda (v) (setq new-value v))))
        ;; Find the field widget and modify it
        (goto-char (point-min))
        (widget-forward 1)
        (let ((widget (widget-at (point))))
          (widget-value-set widget "changed")
          (widget-apply widget :notify widget))
        (expect new-value :to-equal "changed")))))

(describe "defcomponent"
  (it "defines a component and registers it"
    (defcomponent test-simple ()
      :render (vui-text "simple"))
    (expect (vui--get-component 'test-simple) :to-be-truthy))

  (it "defines component with props"
    (defcomponent test-greeting (name)
      :render (vui-text (format "Hello, %s!" name)))
    (let ((def (vui--get-component 'test-greeting)))
      (expect (vui-component-def-props-spec def) :to-equal '(name))))

  (it "defines component with state"
    (defcomponent test-counter ()
      :state ((count 0))
      :render (vui-text (number-to-string count)))
    (let ((def (vui--get-component 'test-counter)))
      (expect (vui-component-def-initial-state-fn def) :to-be-truthy))))

(describe "vui-component"
  (it "creates a component vnode"
    (let ((vnode (vui-component 'test-simple)))
      (expect (vui-vnode-component-p vnode) :to-be-truthy)
      (expect (vui-vnode-component-type vnode) :to-equal 'test-simple)))

  (it "accepts props"
    (let ((vnode (vui-component 'test-greeting :name "World")))
      (expect (plist-get (vui-vnode-component-props vnode) :name)
              :to-equal "World")))

  (it "accepts children"
    (let ((vnode (vui-component 'some-comp :children (list (vui-text "child")))))
      (expect (vui-vnode-component-children vnode) :to-be-truthy))))

(describe "component rendering"
  (it "renders a simple component"
    (defcomponent render-test ()
      :render (vui-text "rendered!"))
    (with-temp-buffer
      (vui-render (vui-component 'render-test))
      (expect (buffer-string) :to-equal "rendered!")))

  (it "renders component with props"
    (defcomponent greeting-test (name)
      :render (vui-text (format "Hi, %s!" name)))
    (with-temp-buffer
      (vui-render (vui-component 'greeting-test :name "Alice"))
      (expect (buffer-string) :to-equal "Hi, Alice!")))

  (it "renders nested components"
    (defcomponent inner-comp ()
      :render (vui-text "[inner]"))
    (defcomponent outer-comp ()
      :render (vui-fragment
               (vui-text "outer:")
               (vui-component 'inner-comp)))
    (with-temp-buffer
      (vui-render (vui-component 'outer-comp))
      (expect (buffer-string) :to-equal "outer:[inner]")))

  (it "passes children to component"
    (defcomponent wrapper-comp ()
      :render (vui-fragment
               (vui-text "[")
               (vui-vnode-fragment--create :children children)
               (vui-text "]")))
    (with-temp-buffer
      (vui-render (vui-component 'wrapper-comp
                                 :children (list (vui-text "content"))))
      (expect (buffer-string) :to-equal "[content]"))))

(describe "lifecycle hooks"
  (it "calls on-mount after first render"
    (let ((mounted nil))
      (defcomponent mount-test ()
        :on-mount (setq mounted t)
        :render (vui-text "hello"))
      (let ((instance (vui-mount (vui-component 'mount-test) "*test-mount*")))
        (unwind-protect
            (progn
              (expect mounted :to-be-truthy)
              (expect (buffer-string) :to-equal "hello"))
          (kill-buffer "*test-mount*")))))

  (it "does not call on-mount on re-render"
    (let ((mount-count 0))
      (defcomponent mount-once ()
        :state ((count 0))
        :on-mount (setq mount-count (1+ mount-count))
        :render (vui-text (number-to-string count)))
      (let ((instance (vui-mount (vui-component 'mount-once) "*test-mount2*")))
        (unwind-protect
            (progn
              (expect mount-count :to-equal 1)
              ;; Trigger re-render
              (vui--rerender-instance instance)
              (expect mount-count :to-equal 1))
          (kill-buffer "*test-mount2*")))))

  (it "calls on-unmount when component is removed"
    (let ((unmounted nil)
          (show-child t))
      (defcomponent unmount-child ()
        :on-unmount (setq unmounted t)
        :render (vui-text "child"))
      (defcomponent unmount-parent ()
        :render (if show-child
                    (vui-component 'unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'unmount-parent) "*test-unmount*")))
        (unwind-protect
            (progn
              (expect unmounted :to-be nil)
              (expect (buffer-string) :to-equal "child")
              ;; Remove child by setting show-child to nil and re-render
              (setq show-child nil)
              (vui--rerender-instance instance)
              (expect unmounted :to-be-truthy)
              (expect (buffer-string) :to-equal "no child"))
          (kill-buffer "*test-unmount*")))))

  (it "calls on-unmount for nested children depth-first"
    (let ((unmount-order nil))
      (defcomponent nested-inner ()
        :on-unmount (push 'inner unmount-order)
        :render (vui-text "inner"))
      (defcomponent nested-outer ()
        :on-unmount (push 'outer unmount-order)
        :render (vui-fragment
                 (vui-text "outer:")
                 (vui-component 'nested-inner)))
      (defcomponent nested-root ()
        :state ((show t))
        :render (if show
                    (vui-component 'nested-outer)
                  (vui-text "empty")))
      (let ((instance (vui-mount (vui-component 'nested-root) "*test-nested*")))
        (unwind-protect
            (progn
              ;; Remove the nested components
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :show nil))
              (vui--rerender-instance instance)
              ;; Inner should unmount before outer (depth-first)
              (expect unmount-order :to-equal '(outer inner)))
          (kill-buffer "*test-nested*")))))

  (it "provides props and state to lifecycle hooks"
    (let ((mount-props nil)
          (mount-state nil))
      (defcomponent lifecycle-args (name)
        :state ((count 42))
        :on-mount (setq mount-props name
                        mount-state count)
        :render (vui-text name))
      (let ((instance (vui-mount (vui-component 'lifecycle-args :name "test")
                                 "*test-args*")))
        (unwind-protect
            (progn
              (expect mount-props :to-equal "test")
              (expect mount-state :to-equal 42))
          (kill-buffer "*test-args*"))))))

(describe "reconciliation"
  (it "preserves child component state across parent re-render"
    ;; Define a child component with state
    (defcomponent stateful-child ()
      :state ((value 42))
      :render (vui-text (number-to-string value)))
    ;; Define parent that renders the child
    (defcomponent parent-comp ()
      :state ((label "test"))
      :render (vui-fragment
               (vui-text label)
               (vui-text ":")
               (vui-component 'stateful-child)))
    ;; Mount and verify initial render
    (let* ((instance (vui-mount (vui-component 'parent-comp) "*test-recon*")))
      (unwind-protect
          (progn
            (expect (buffer-string) :to-equal "test:42")
            ;; Get the child instance and change its state
            (let ((child (car (vui-instance-children instance))))
              (expect child :to-be-truthy)
              ;; Manually set child state
              (setf (vui-instance-state child)
                    (plist-put (vui-instance-state child) :value 100)))
            ;; Re-render parent (simulating state change)
            (vui--rerender-instance instance)
            ;; Child state should be preserved
            (expect (buffer-string) :to-equal "test:100"))
        (kill-buffer "*test-recon*"))))

  (it "reuses child by index when no key provided"
    (defcomponent indexed-child (label)
      :state ((count 0))
      :render (vui-text (format "%s:%d" label count)))
    (defcomponent indexed-parent ()
      :render (vui-fragment
               (vui-component 'indexed-child :label "A")
               (vui-component 'indexed-child :label "B")))
    (let ((instance (vui-mount (vui-component 'indexed-parent) "*test-idx*")))
      (unwind-protect
          (progn
            (expect (buffer-string) :to-equal "A:0B:0")
            ;; Set state on first child
            (let ((first-child (car (vui-instance-children instance))))
              (setf (vui-instance-state first-child)
                    (plist-put (vui-instance-state first-child) :count 5)))
            (vui--rerender-instance instance)
            ;; First child should preserve state, second should stay 0
            (expect (buffer-string) :to-equal "A:5B:0"))
        (kill-buffer "*test-idx*")))))

;;; vui-test.el ends here
