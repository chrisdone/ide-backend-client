;;; stack-ide.el --- A minor mode enabling various features based on stack-ide.

;; Copyright (c) 2015 Chris Done.

;; URL: https://github.com/commercialhaskell/stack-ide
;; Keywords: Haskell, stack
;; Package-Requires: ((haskell-mode "13.14") (cl-lib "0.5") (flycheck "0.23"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;   'stack-ide' is a minor mode enabling various features based on
;;   the 'stack-ide' external process.
;;
;;   Features:
;;
;;       'M-.'      - Go to definition of thing at point.
;;       'C-c C-k'  - Clear the interaction buffer.
;;       'C-c C-t'  - Display type info of thing at point.
;;       'C-c C-i'  - Display the info of the thing at point.
;;       'C-c C-l'  - Load the current buffer's file.
;;
;;       'stack-ide' also integrates with Flycheck for on-the-fly GHC
;;       compiler error and HLint warning reporting.
;;
;;   Conceptual Overview:
;;
;;       'stack-ide' minor mode is a combination of two external
;;       processes, 'ide-backend' and 'stack', wrapped up into the
;;       'stack-ide' process. 'ide-backend' drives the GHC API to
;;       build, query, and run your code. 'stack' is a cross-platform
;;       program for developing Haskell projects.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'haskell-mode)
(require 'haskell-cabal)
(require 'cl-lib)
(require 'stack-fifo)
(require 'flycheck)
(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

;;;###autoload
(define-minor-mode stack-ide
  "A minor mode enabling various features based on stack-ide.

Automatically starts and stops flycheck-mode when you
enable/disable it. It makes this assumption in the interest of
easier user experience. Disable with `stack-ide-manage-flycheck'."
  :lighter " Stack"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-.") 'stack-ide-goto)
            (define-key map (kbd "C-c C-k") 'stack-ide-clear)
            (define-key map (kbd "C-c C-t") 'stack-ide-type)
            (define-key map (kbd "C-c C-i") 'stack-ide-info)
            (define-key map (kbd "C-c C-l") 'stack-ide-load)
            map)
  (when (buffer-file-name)
    (if stack-ide
        (progn (when (bound-and-true-p interactive-haskell-mode)
                 (when (y-or-n-p "interactive-haskell-mode is enabled. Disable it?")
                   (interactive-haskell-mode -1)))
               (when stack-ide-manage-flycheck
                 (flycheck-mode 1)
                 (flycheck-disable-checker 'haskell-ghc)
                 (flycheck-select-checker 'stack-ide)
                 (flycheck-buffer)))
      (when stack-ide-manage-flycheck
        (flycheck-mode -1)))))

(define-derived-mode inferior-stack-ide fundamental-mode "Inferior-Stack-IDE"
  "Major mode for interacting with an inferior stack-ide process.")

(define-key inferior-stack-ide-map (kbd "C-c C-c") 'stack-ide-stop)
(define-key inferior-stack-ide-map (kbd "C-c C-k") 'stack-ide-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup stack-ide nil
  "IDE backend support for Haskell."
  :group 'haskell)

(defcustom stack-ide-proc-path
  "stack"
  "Path to the stack executable."
  :type 'string
  :group 'stack-ide)

(defcustom stack-ide-manage-flycheck
  t
  "Automatically start and stop flycheck when the minor mode is
enabled/disabled."
  :type 'boolean
  :group 'stack-ide)

(defcustom stack-ide-print-error-messages
  nil
  "Print error messages after loading the project?"
  :type 'boolean
  :group 'stack-ide)

(defcustom stack-ide-show-popup
  nil
  "Show type and info messages in a popup?"
  :type 'boolean
  :group 'stack-ide)

(defvar stack-ide-queue nil)
(make-variable-buffer-local 'stack-ide-queue)

(defvar stack-ide-back-queue nil)
(make-variable-buffer-local 'stack-ide-back-queue)

(defvar stack-ide-buffer nil)
(make-variable-buffer-local 'stack-ide-buffer)

(defvar stack-ide-name nil)
(make-variable-buffer-local 'stack-ide-name)

(defvar stack-ide-tried-to-start nil)
(make-variable-buffer-local 'stack-ide-tried-to-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;;;###autoload
(defun stack-ide-status ()
  "Print the status of the current stack process."
  (interactive)
  (if (stack-ide-buffer)
      (if (stack-ide-process)
          (if (process-live-p (stack-ide-process))
              (message "The process is live.")
            (message "There is a Stack process, but it's dead."))
        (message "There is a stack buffer, but no Stack process."))
    (message "There is no Stack buffer.")))

;;;###autoload
(defun stack-ide-start ()
  "Start an inferior process and buffer."
  (interactive)
  (if (stack-ide-live-p)
      (switch-to-buffer (stack-ide-buffer))
    (with-current-buffer (stack-ide-buffer)
      (setq buffer-read-only t)
      (inferior-stack-ide)
      (stack-ide-set-initial-command)
      (setq stack-ide-buffer "")
      (let* ((project-name (stack-ide-name))
             (name (stack-ide-process-name project-name))
             (args (append (list name
                                 nil
                                 stack-ide-proc-path
                                 "ide"
                                 "start")
                           (list project-name)))
             (process (or (get-process name)
                          (progn (stack-ide-log "Starting: %S" args)
                                 (apply #'start-process
                                        args)))))
        (set-process-sentinel process 'stack-ide-sentinel)
        (set-process-filter process 'stack-ide-filter)))))

(defun stack-ide-set-initial-command ()
  "Set the initial command callback. The `stack ide` command will
reload targets on start-up, so that's the default command we'll
start with."
  (setq stack-ide-current-command
        (list :json nil
              :data nil
              :cont 'stack-ide-loading-callback
              :label nil))
  (setq stack-ide-queue (stack-fifo-make))
  (stack-ide-log "Set initial command."))

(defun stack-ide-stop ()
  "Stop the process."
  (interactive)
  (with-current-buffer (stack-ide-buffer)
    (when (stack-ide-process)
      (setq stack-ide-current-command nil)
      (setq stack-ide-buffer "")
      (kill-process (stack-ide-process))
      (delete-process (stack-ide-process)))))

(defun stack-ide-reset ()
  "Reset the process."
  (interactive)
  (with-current-buffer (stack-ide-buffer)
    (when (stack-ide-process)
      (setq stack-ide-current-command nil)
      (setq stack-ide-buffer "")
      (setq stack-ide-queue (stack-fifo-make)))))

(defun stack-ide-restart ()
  "Restart the process with a fresh command queue."
  (interactive)
  (stack-ide-stop)
  (stack-ide-start))

(defun stack-ide-live-p ()
  "Is the process alive?"
  (and (stack-ide-process)
       (process-live-p (stack-ide-process))))

(defun stack-ide-clear ()
  "Clear the interaction buffer."
  (interactive)
  (with-current-buffer (stack-ide-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun stack-ide-load ()
  "Load the current buffer's file."
  (interactive)
  (save-buffer)
  (with-current-buffer (stack-ide-buffer)
    (stack-ide-reload)))

(defun stack-ide-goto ()
  "Go to definition of thing at point."
  (interactive)
  (let ((filename (buffer-file-name))
        (module-name (haskell-guess-module-name))
        (span (stack-ide-span)))
    (let* ((span-info
            (stack-ide-get-span-info
             module-name
             (with-current-buffer (stack-ide-buffer)
               (file-relative-name filename default-directory))
             span))
           (infos
            (stack-contents
             span-info))
           (_ (when (and (vectorp infos) (= 0 (length infos)))
                (error "Couldn't find location for this. Is the module loaded in the backend?
Run `M-x stack-ide-list-loaded-modules' to see what's loaded.")))
           (parts (mapcar #'identity (elt infos 0)))
           (info (stack-contents (elt parts 0)))
           (span (elt parts 1))
           (scope (stack-lookup 'tag (stack-lookup 'idScope info)))
           (def-span (stack-lookup-contents
                      'idDefSpan
                      (stack-lookup 'idProp info))))
      (cond
       ((listp def-span)
        (stack-ide-goto-span def-span))
       (t
        (let* ((imported-from
                (stack-lookup
                 'idImportedFrom
                 (stack-lookup 'idScope info)))
               (imported-module (stack-lookup 'moduleName imported-from))
               (defined-in (stack-lookup
                            'idDefinedIn
                            (stack-lookup 'idProp info)))
               (package (stack-lookup 'modulePackage defined-in))
               (package-name (stack-lookup 'packageName package))
               (package-ver (stack-lookup 'packageVersion package))
               (module (stack-lookup 'moduleName defined-in)))
          (message "Imported via %s, defined in %s (%s-%s)"
                   (haskell-fontify-as-mode imported-module 'haskell-mode)
                   (haskell-fontify-as-mode module 'haskell-mode)
                   package-name
                   package-ver)))))))

(defun stack-ide-list-loaded-modules ()
  "List the loaded modules in the backend."
  (interactive)
  (let ((modules
         (stack-contents
          (with-current-buffer (stack-ide-buffer)
            (stack-ide-call
             `((tag . "RequestGetLoadedModules")
               (contents
                . [])))))))
    (pop-to-buffer (stack-ide-buffer))
    (stack-ide-log "Loaded modules: %s"
                    (mapconcat #'identity
                               (sort (mapcar #'identity modules) #'string<)
                               "\n"))))

(defun stack-ide-info ()
  "Display the info of the thing at point."
  (interactive)
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-ide-points))
         (orig (point))
         (span (stack-ide-span-from-points (car points)
                                            (cdr points)))
         (info (stack-ide-get-span-info
                module-name
                (with-current-buffer (stack-ide-buffer)
                  (file-relative-name filename default-directory))
                span))
         (info-contents (stack-contents (elt (elt (stack-contents info) 0) 0)))
         (scope (stack-lookup 'idScope info-contents))
         (prop (stack-lookup 'idProp info-contents))
         (qual (stack-lookup 'idImportQual scope))
         (from (stack-lookup 'idImportedFrom scope))
         (span (stack-lookup 'idImportSpan scope))

         (space (stack-lookup 'idSpace prop))
         (idDefSpan (stack-lookup 'idDefSpan prop))
         (idDefinedIn (stack-lookup 'idDefinedIn prop))
         (modulePackage (stack-lookup 'modulePackage idDefinedIn))
         (moduleName (stack-lookup 'moduleName idDefinedIn))
         (packageVersion (stack-lookup 'packageVersion modulePackage))
         (packageKey (stack-lookup 'packageKey modulePackage))
         (packageName (stack-lookup 'packageKey modulePackage))
         (idType (stack-lookup 'idType prop))
         (idName (stack-lookup 'idName prop)))
    (let ((info-string (concat
                        "Identifier: " (haskell-fontify-as-mode idName 'haskell-mode) "\n"
                        "Type: " (haskell-fontify-as-mode idType 'haskell-mode) "\n"
                        "Module: " (haskell-fontify-as-mode moduleName 'haskell-mode) "\n"
                        "Package: "  (if (string= "main" packageName)
                                         "(this one)"
                                       packageName))))
      (cond ((and stack-ide-show-popup (fboundp 'popup-tip))
             (popup-tip info-string))
            (t (message info-string))))))

(defun stack-ide-type (&optional insert-value)
  "Display type info of thing at point."
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-ide-points))
         (orig (point))
         (span (stack-ide-span-from-points (car points)
                                            (cdr points))))
    (let* ((types (stack-contents
                   (stack-ide-get-exp-types
                    module-name
                    (with-current-buffer (stack-ide-buffer)
                      (file-relative-name filename default-directory))
                    span)))
           (types (mapcar #'identity types))
           (code (buffer-substring-no-properties
                  (car points)
                  (cdr points)))
           (type (stack-contents (car types)))
           (ty (stack-lookup 'text type)))
      (if insert-value
          (let ((ident-pos (haskell-ident-pos-at-point)))
            (cond
             ((region-active-p)
              (delete-region (region-beginning)
                             (region-end))
              (insert "(" code " :: " ty ")")
              (goto-char (1+ orig)))
             ((= (line-beginning-position) (car ident-pos))
              (goto-char (line-beginning-position))
              (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)
                      "\n"))
             (t
              (save-excursion
                (goto-char (car ident-pos))
                (let ((col (current-column)))
                  (save-excursion (insert "\n")
                                  (indent-to col))
                  (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)))))))
        (unless (null types)
          (let ((type-string (format "%s"
                                     (mapconcat (lambda (type)
                                                  (haskell-fontify-as-mode
                                                   (concat
                                                    code
                                                    " :: "
                                                    (elt type 0))
                                                   'haskell-mode))
                                                (cl-subseq types 0 1)
                                                "\n"))))
            (cond ((and stack-ide-show-popup (fboundp 'popup-tip)) (popup-tip type-string))
                  (t (message type-string)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process filters and sentinel

(defun stack-ide-filter (process response)
  (with-current-buffer (stack-ide-buffer (stack-ide-name-from-process process))
    (if stack-ide-current-command
        (let* ((lines (split-string (concat stack-ide-buffer response) "\n")))
          (setq stack-ide-buffer (car (last lines)))
          (setq lines (butlast lines))
          (let ((data (plist-get stack-ide-current-command :data))
                (cont (plist-get stack-ide-current-command :cont)))
            (while lines
              (let ((line (pop lines)))
                (stack-ide-log
                 "%s <- %s"
                 (if (plist-get stack-ide-current-command :label)
                     (format "[%s]" (plist-get stack-ide-current-command :label))
                   "")
                 (haskell-fontify-as-mode line 'javascript-mode))
                (when (let* ((error-msg nil)
                             (json (condition-case e
                                       (json-read-from-string line)
                                     (error "Problem reading JSON from server, probably an error message:\n%s" line)))
                             (ret (condition-case e
                                      (funcall cont data json)
                                    (error (setq error-msg e)
                                           :error))))
                        (cl-ecase ret
                          (:done t)
                          (:continue nil)
                          (:error
                           (setq stack-ide-buffer "")
                           (setq stack-ide-current-command nil)
                           (setq stack-ide-queue nil)
                           (error "Command handler error: %S\n\nThe command queue has been cleared."
                                  error-msg))
                          (t
                           (error "A command handler must return either :done or :continue,
but it returned: %S
command was: %S" ret stack-ide-current-command))))
                  (cl-loop for line in lines
                           do (stack-ide-log
                               "Extraneous lines after command completed: %s"
                               (haskell-fontify-as-mode line 'javascript-mode)))
                  (setq stack-ide-current-command nil)
                  (setq lines nil)
                  (stack-ide-queue-trigger))))))
      (stack-ide-log "Ignoring: %s"
                      (haskell-fontify-as-mode response 'javascript-mode)))))

(defun stack-ide-sentinel (process event)
  (with-current-buffer (stack-ide-buffer (stack-ide-name-from-process process))
    (stack-ide-log "Process event: %s" event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command queue

(defvar stack-ide-current-command nil
  "Current command handler.")
(make-variable-buffer-local 'stack-ide-current-command)

(defvar stack-ide-buffer ""
  "A buffer for the process.")
(make-variable-buffer-local 'stack-ide-buffer)

(defvar stack-ide-queue nil
  "Command queue.")
(make-variable-buffer-local 'stack-ide-queue)

(defun stack-ide-queue ()
  "Get the FIFO queue of this process."
  (or stack-ide-queue
      (setq stack-ide-queue (stack-fifo-make))))

(defun stack-ide-back-queue ()
  "Get the FIFO back queue of this process."
  (or stack-ide-back-queue
      (setq stack-ide-back-queue (stack-fifo-make))))

(defun stack-ide-enqueue-front (json data cont &optional label)
  "Enqueue a JSON command to the command queue, calling (CONT
DATA line) for each response line until CONT returns nil. This is
the first priority queue, anything pushed to this queue will be
run before anything in the back queue."
  (cond
   ((stack-ide-live-p)
    (stack-ide-log "[%s] => %s" label (haskell-fontify-as-mode (json-encode json) 'javascript-mode))
    (stack-fifo-push (stack-ide-queue)
                     (list :json json :data data :cont cont :label label))
    (stack-ide-queue-trigger))
   (t (stack-ide-try-start))))

(defun stack-ide-enqueue (json data cont &optional label)
  "Same as `stack-ide-front', but puts it on the back
queue. Items are only moved onto the front queue when the front
queue is empty. This lets a command which consists of a few back
and forth steps to continue its processing uninterrupted."
  (cond
   ((stack-ide-live-p)
    (stack-ide-log "[%s] ~> %s" label (haskell-fontify-as-mode (json-encode json) 'javascript-mode))
    (stack-fifo-push (stack-ide-back-queue)
                     (list :json json :data data :cont cont :label label))
    (stack-ide-queue-trigger))
   (t (stack-ide-try-start))))

(defun stack-ide-try-start ()
  "Try to start, but only try once."
  (cond
   ((not stack-ide-tried-to-start)
    (setq stack-ide-tried-to-start t)
    (message "Starting a Stack IDE backend process for this project: %s, stack directory: %s"
             (stack-ide-cabal-name)
             (stack-ide-dir))
    (stack-ide-start))
   (t (message "Attempted to run a Stack IDE command, but the server isn't started. We already tried once this session. Run `M-x stack-ide-restart` to confirm that you want to start it."))))

(defun stack-ide-call (json)
  "Call a JSON command. Wait for any existing queued commands to
complete, then sends the request, blocking on the
response. Returns the response."
  (let ((data (list nil)))
    (stack-ide-enqueue
     json data
     (lambda (data reply)
       (setcar data reply)
       :done))
    (stack-ide-queue-flush)
    (car-safe data)))

(defun stack-ide-queue-processed-p ()
  "Return t if command queue has been completely processed."
  (and (stack-fifo-null-p stack-ide-queue)
       (null stack-ide-current-command)))

(defun stack-ide-queue-flush ()
  "Block till PROCESS's command queue has been completely processed.
This uses `accept-process-output' internally."
  (let ((proc (stack-ide-process)))
    (while (not (stack-ide-queue-processed-p))
      (stack-ide-queue-trigger)
      (accept-process-output proc 1))))

(defun stack-ide-queue-trigger ()
  "Trigger the next command in the queue if there is no current
command."
  (if stack-ide-current-command
      (unless (stack-fifo-null-p (stack-ide-queue))
        (stack-ide-log "Stack command queue is currently active, waiting ..."))
    (when (stack-fifo-null-p (stack-ide-queue))
      (stack-ide-log "Command queue is now empty.")
      (unless (stack-fifo-null-p (stack-ide-back-queue))
        (stack-ide-log "Pushing next item from back queue to front queue ...")
        (stack-fifo-push (stack-ide-queue)
                   (stack-fifo-pop (stack-ide-back-queue)))))
    (unless (stack-fifo-null-p (stack-ide-queue))
      (setq stack-ide-current-command
            (stack-fifo-pop (stack-ide-queue)))
      (stack-ide-log
       "[%S] -> %s"
       (plist-get stack-ide-current-command :label)
       (haskell-fontify-as-mode
        (json-encode (plist-get stack-ide-current-command :json))
        'javascript-mode))
      (process-send-string
       (stack-ide-process)
       (concat (json-encode (plist-get stack-ide-current-command :json))
               "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project functions

(defun stack-ide-packages ()
  "Get packages for the Stack configuration."
  (split-string (shell-command-to-string "stack ide packages") "\n" t))

(defun stack-ide-process ()
  "Get the current process."
  (get-process (stack-ide-process-name (stack-ide-name))))

(defun stack-ide-buffer (&optional name)
  "The inferior buffer."
  (let ((default-directory (stack-ide-dir)))
    (get-buffer-create
     (stack-ide-buffer-name
      (or name
          (stack-ide-name))))))

(defun stack-ide-name-from-process (proc)
  "Get the name of the project from the process."
  (substring (process-name proc) (length "stack:")))

(defun stack-ide-process-name (name)
  "Name for the inferior process."
  (format "stack:%s"
          name))

(defun stack-ide-buffer-name (name)
  "Name for the inferior buffer."
  (format "*stack:%s*"
          name))

(defun stack-ide-dir ()
  "The directory for the project."
  (file-name-directory (haskell-cabal-find-file)))

(defun stack-ide-name ()
  "The name for the current project based on the current
directory."
  (or stack-ide-name
      (setq stack-ide-name
            (stack-ide-cabal-name))))

(defun stack-ide-cabal-name ()
  "Get the name of the session to use, based on the cabal file."
  (let ((cabal-file (haskell-cabal-find-file)))
    (if (string-match "\\([^\\/]+\\)\\.cabal$" cabal-file)
        (let ((name (match-string 1 cabal-file)))
          (when (not (member name (stack-ide-packages)))
            (message "This cabal project “%s” isn't in your stack.yaml configuration." name))
          name)
      (progn (message "Couldn't figure out cabal file, assuming no project.")
             nil))))

(defun stack-ide-log (&rest args)
  "Log a string to the inferior buffer."
  (with-current-buffer (stack-ide-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (apply #'format args)
              "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun stack-ide-reload ()
  "Compile the code and fetch compile errors."
  (with-current-buffer (stack-ide-buffer)
    (stack-ide-enqueue
     `((tag . "RequestUpdateSession")
       (contents . []))
     nil
     'stack-ide-loading-callback)))

;; (defun stack-ide-load-buffer ()
;;   "Compile the code and fetch compile errors."
;;   (interactive)
;;   (with-current-buffer (stack-ide-buffer)
;;     (stack-ide-enqueue
;;      `((tag . "RequestUpdateSession")
;;        (contents . [((tag . "RequestUpdateTargets")
;;                      (contents . ((tag . "TargetsInclude")
;;                                   (contents . ["src/Stack/Package.hs"]))))]))
;;      nil
;;      'stack-ide-loading-callback)))

(defun stack-ide-get-span-info (module file span)
  "Get the span info of the given location."
  (with-current-buffer (stack-ide-buffer)
    (stack-ide-call
     `((tag . "RequestGetSpanInfo")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-ide-get-exp-types (module file span)
  "Get the type info of the given location."
  (with-current-buffer (stack-ide-buffer)
    (stack-ide-call
     `((tag . "RequestGetExpTypes")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-ide-get-use-sites (module file span)
  "Get all uses of an identifier."
  )

(defun stack-ide-get-completions (module string)
  "Get all uses of an identifier."
  )

(defun stack-ide-loading-callback (_ reply)
  "Callback for when loading modules."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseUpdateSession")
      (let* ((contents (stack-contents reply))
             (tag (stack-tag contents)))
        (cond
         ((string= tag "UpdateStatusProgress")
          (stack-ide-progress-callback _ reply)
          :continue)
         ((string= tag "UpdateStatusDone")
          (stack-ide-enqueue-front
           `((tag . "RequestGetSourceErrors")
             (contents . []))
           nil
           'stack-ide-get-source-errors-callback)
          :done)
         (t :continue))))
     (t
      :continue))))

(defun stack-ide-progress-callback (_ reply)
  "Callback for status reports. Utilized in multiple places."
  (let* ((contents (stack-contents reply))
         (update (stack-contents contents))
         (step (stack-lookup 'progressStep update))
         (total (stack-lookup 'progressNumSteps update))
         (msg (stack-lookup 'progressParsedMsg update)))
    (message "[%s/%s] %s"
             (propertize (number-to-string step) 'face 'compilation-line-number)
             (propertize (number-to-string total) 'face 'compilation-line-number)
             msg)))

(defun stack-ide-get-source-errors-callback (_ reply)
  "Handle the reply from getting source errors."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseGetSourceErrors")
      (let ((any-errors nil)
            (warnings 0))
        (cl-loop
         for item in (mapcar #'identity (stack-contents reply))
         do (let* ((kind (stack-lookup 'errorKind item))
                   (span (stack-contents (stack-lookup 'errorSpan item)))
                   (msg (stack-lookup 'errorMsg item))
                   (fp (stack-lookup 'spanFilePath span))
                   (sl (stack-lookup 'spanFromLine span))
                   (sc (stack-lookup 'spanFromColumn span))
                   (el (stack-lookup 'spanToLine span))
                   (ec (stack-lookup 'spanToColumn span)))
              (cond ((string= kind "KindError")
                     (setq any-errors t))
                    ((string= kind "KindWarning")
                     (setq warnings (1+ warnings))))
              (when
                  stack-ide-print-error-messages
                (message "%s"
                         (propertize
                          (format "%s:(%d,%d)-(%d,%d): \n%s"
                                  fp sl sc el ec msg)
                          'face
                          (cond
                           ((string= kind "KindWarning")
                            'compilation-warning)
                           ((string= kind "KindError")
                            'compilation-error)))))))
        (unless any-errors
          (if (= 0 warnings)
              (message "OK.")
            (message (propertize "OK (%d warning%s)." 'face 'compilation-warning)
                     warnings
                     (if (= 1 warnings) "" "s")))))
      :done)
     (t :done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Span functions

(defun stack-ide-points ()
  "Get the current points; either a selected region or an
identifier's points."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((ident (haskell-ident-pos-at-point)))
      (cons (car ident)
            (cdr ident)))))

(defun stack-ide-span-from-points (beg end)
  "Get the span representation for the span from BEG to END."
  (save-excursion
    (list :sl (progn (goto-char beg)
                     (line-number-at-pos))
          :sc (1+ (current-column))
          :el (progn (goto-char end)
                     (line-number-at-pos))
          :ec (1+ (current-column)))))

(defun stack-ide-span ()
  "Get the span from the haskell points."
  (let ((points (or (haskell-spanable-pos-at-point)
                    (haskell-ident-pos-at-point)
                    (stack-ide-loose-ident-at-point))))
    (if points
        (stack-ide-span-from-points (car points) (cdr points))
      (error "No identifier at point."))))

(defun stack-ide-goto-span (span)
  "Get buffer points from a span."
  (with-current-buffer (stack-ide-buffer)
    (find-file (stack-lookup 'spanFilePath span))
    (goto-char (point-min))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (1- (stack-lookup 'spanFromLine span)))
      (goto-char (line-beginning-position))
      (forward-char (1- (stack-lookup 'spanFromColumn span))))))

(defun stack-ide-loose-ident-at-point ()
  "Get the loose ident at point."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON helpers

(defun stack-ide-list->hashtable (xs)
  "Convert a list to a hashtable."
  (let ((h (make-hash-table)))
    (cl-loop for (key . val)
             in xs
             do (puthash key val h))
    h))

(defun stack-lookup (key object)
  "Get from a JSON object."
  (cdr (assoc key (mapcar #'identity object))))

(defun stack-contents (object)
  "Get from a JSON object."
  (stack-lookup 'contents object))

(defun stack-tag (object)
  "Get the tag of an object."
  (stack-lookup 'tag object))

(defun stack-lookup-contents (key object)
  "Get from a JSON object."
  (stack-contents (stack-lookup key object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defun stack-ide-flycheck-start (checker flycheck-callback &optional buffer attempt-count)
  "Run a compile on demand, triggered by Flycheck."
  (when buffer (set-buffer buffer))
  (let ((max-attempts 2))
    (if (not (stack-ide-live-p))
        (if (> (or attempt-count 0) max-attempts)
            (stack-ide-log "Stack backend isn't ready for Flycheck use. Giving up (waited %d seconds)."
                            max-attempts)
          (stack-ide-log "Stack backend isn't ready. Waiting (%d attempts) ..."
                          (or attempt-count 0))
          (progn (stack-ide-log "Flycheck tried to use the Stack backend, but the Stack backend isn't started yet. Starting it ...")
                 (stack-ide-try-start)
                 (run-with-idle-timer 1 nil 'stack-ide-flycheck-start checker flycheck-callback
                                      (current-buffer)
                                      (1+ (or attempt-count 0)))))
      (progn (stack-ide-log "Running Flycheck with Stack backend ...")
             (write-region (point-min) (point-max) (buffer-file-name))
             (clear-visited-file-modtime)
             (let ((source-buffer (current-buffer))
                   (label (format "flycheck %s" (buffer-name (current-buffer)))))
               (with-current-buffer (stack-ide-buffer)
                 (stack-ide-enqueue
                  `((tag . "RequestUpdateSession")
                    (contents . []))
                  (list :flycheck-callback flycheck-callback
                        :stack-buffer (current-buffer)
                        :source-buffer source-buffer
                        :label label)
                  'stack-ide-flycheck-callback
                  label)))))))

(defun stack-ide-flycheck-callback (state reply)
  "Callback for the flycheck loading. Once done, it will report
  errors/warnings to CALLBACK."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseUpdateSession")
      (let* ((contents (stack-contents reply))
             (tag (stack-tag contents)))
        (cond
         ((string= tag "UpdateStatusProgress")
          (stack-ide-progress-callback nil reply)
          :continue)
         ((string= tag "UpdateStatusDone")
          (stack-ide-enqueue-front
           `((tag . "RequestGetSourceErrors")
             (contents . []))
           state
           'stack-ide-flycheck-errors-callback
           (plist-get state :label))
          :done)
         (t :continue))))
     (t
      :continue))))

(defun stack-ide-flycheck-errors-callback (state reply)
  "Collect error messages and pass them to FLYCHECK-CALLBACK."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseGetSourceErrors")
      (let ((messages (list)))
        (cl-loop
         for item in (mapcar #'identity (stack-contents reply))
         do (let* ((kind (stack-lookup 'errorKind item))
                   (span (stack-contents (stack-lookup 'errorSpan item)))
                   (msg (stack-lookup 'errorMsg item))
                   (filename (stack-lookup 'spanFilePath span))
                   (sl (stack-lookup 'spanFromLine span))
                   (sc (stack-lookup 'spanFromColumn span))
                   (el (stack-lookup 'spanToLine span))
                   (ec (stack-lookup 'spanToColumn span)))
              (let ((orig (current-buffer))
                    (buffer
                     (with-current-buffer (plist-get state :stack-buffer)
                       (let ((value (get-file-buffer filename)))
                             (if (listp value)
                                 (car value)
                               value)))))
                (if (not (null buffer))
                 (add-to-list
                  'messages
                  (flycheck-error-new-at
                   sl sc
                   (cond
                    ((string= kind "KindWarning") 'warning)
                    ((string= kind "KindError") 'error)
                    (t (message "kind: %s" kind)'error))
                   msg
                   :checker 'stack-ide
                   :buffer buffer)
                  t))
                (set-buffer orig))))
        ;; Calling it asynchronously is necessary for flycheck to
        ;; work properly. See
        ;; <https://github.com/flycheck/flycheck/pull/524#issuecomment-64947118>
        ;;
        ;; Also, the `stack-ide-call-in-buffer' utility is also
        ;; needed because the reply needs to be called in the same
        ;; buffer.
        (run-with-idle-timer 0
                             nil
                             'stack-ide-call-in-buffer
                             (plist-get state :source-buffer)
                             (plist-get state :flycheck-callback)
                             'finished
                             messages)
        (message "Flycheck done."))
      :done)
     (t :done))))

(defun stack-ide-call-in-buffer (buffer func &rest args)
  "Utility function which calls FUNC in BUFFER with ARGS."
  (with-current-buffer buffer
    (apply func args)))

(flycheck-define-generic-checker 'stack-ide
  "A syntax and type checker for Haskell using Stack's IDE support."
  :start 'stack-ide-flycheck-start
  :modes '(haskell-mode)
  :next-checkers '((warning . haskell-hlint)))

(provide 'stack-ide)
;;; stack-ide.el ends here
