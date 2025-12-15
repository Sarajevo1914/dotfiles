;;; performance.el --- Native compilation tuning -*- lexical-binding: t -*-

(when (featurep 'native-compile)
  (setq
  native-comp-async-report-warnings-errors nil
  native-comp-deferred-compilation t
  native-comp-async-jobs-number (max 1 (/ (num-processors) 2)))
  )

(provide 'performance)
