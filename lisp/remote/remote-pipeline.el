;;; remote-pipeline.el --- Ordered transport pipelines -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A pipeline is one logical reachability path containing ordered transport
;; stages and exposing one or more execution backends.  `remote-link' names
;; remain compatibility entry points only.

;;; Code:

(require 'cl-lib)
(require 'remote-core)

(cl-defstruct (remote-pipeline-stage
               (:constructor remote-pipeline-stage-create))
  id transport config capabilities)

(defalias 'remote-pipeline-enabled-p #'remote-pipeline-enabled)

(defun remote-pipeline-stage-normalize (stage index)
  "Return normalized pipeline STAGE at INDEX."
  (cond
   ((remote-pipeline-stage-p stage) stage)
   ((or (stringp stage) (symbolp stage))
    (remote-pipeline-stage-create
     :id (format "stage-%d" index)
     :transport (remote-normalize-id stage)))
   ((listp stage)
    (let ((id (or (plist-get stage :id)
                  (alist-get 'id stage)
                  (format "stage-%d" index)))
          (transport
           (or (plist-get stage :transport)
               (alist-get 'transport stage)
               (plist-get stage :type)
               (alist-get 'type stage))))
      (unless transport
        (error "Pipeline stage has no transport: %S" stage))
      (remote-pipeline-stage-create
       :id (remote-normalize-id id)
       :transport (remote-normalize-id transport)
       :config
       (or (plist-get stage :config)
           (alist-get 'config stage))
       :capabilities
       (or (plist-get stage :capabilities)
           (alist-get 'capabilities stage)))))
   (t (error "Invalid pipeline stage: %S" stage))))

(defun remote-pipeline-stages (pipeline)
  "Return normalized ordered stages belonging to PIPELINE."
  (let* ((config (remote-link-config pipeline))
         (configured
          (or (plist-get config :stages)
              (plist-get config :pipeline))))
    (if configured
        (cl-loop for stage in configured
                 for index from 1
                 collect
                 (remote-pipeline-stage-normalize stage index))
      (list
       (remote-pipeline-stage-create
        :id (remote-link-short-id pipeline)
        :transport
        (or (plist-get config :transport)
            (plist-get config :method)
            (cond
             ((member "native" (remote-link-plugin-ids pipeline))
              "direct")
             ((and
               (plist-get config :host)
               (or
                (member "tramp" (remote-link-plugin-ids pipeline))
                (member "tramp-rpc"
                        (remote-link-plugin-ids pipeline))))
              "ssh")
             (t "direct")))
        :config config
        :capabilities
        (copy-sequence (remote-link-capabilities pipeline)))))))

(cl-defun remote-register-pipeline
    (target-id id backend-ids
               &key stages (enabled t) (priority 0)
               config capabilities source)
  "Register an ordered reachability pipeline for TARGET-ID.
BACKEND-IDS are implementations which can execute over the pipeline."
  (let ((config (copy-sequence config)))
    (when stages
      (setq config
            (plist-put
             config :stages
             (cl-loop for stage in stages
                      for index from 1
                      collect
                      (remote-pipeline-stage-normalize stage index)))))
    (remote-register-link
     target-id id backend-ids
     :enabled enabled
     :priority priority
     :config config
     :capabilities capabilities
     :source source)))

(defun remote-get-pipeline (id &optional target-id)
  "Return pipeline ID, optionally relative to TARGET-ID."
  (remote-get-link id target-id))

(defun remote-pipelines-for-target (target-id)
  "Return registered pipelines for TARGET-ID."
  (remote-links-for-target target-id))

(defun remote-route-pipeline (route)
  "Return the pipeline selected by ROUTE."
  (remote-route-link route))

(defun remote-pipeline-resolve
    (pipeline adapter capability &optional context constraints)
  "Resolve CAPABILITY for ADAPTER through PIPELINE.
CONSTRAINTS are passed to `remote-resolve'; an explicit pipeline constraint
always wins over a constraint inherited from the caller."
  (remote-resolve
   adapter capability context
   (plist-put
    (copy-sequence constraints)
    :link
    (if (remote-pipeline-p pipeline)
        (remote-pipeline-id pipeline)
      pipeline))))

(provide 'remote-pipeline)
;;; remote-pipeline.el ends here
