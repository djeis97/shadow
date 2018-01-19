(in-package :shadow)

(defclass shader-info ()
  ((%programs :reader programs
              :initform (make-hash-table))
   (%buffers :reader buffers
             :initform (make-hash-table))))

(defvar *shader-info* (make-instance 'shader-info))
(defvar *recompilation-hook* (constantly nil))

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun stage-type (stage)
  (varjo.internals::stage-obj-to-name stage))

(defun stage-type->shader-type (stage-type)
  (ecase stage-type
    (:vertex :vertex-shader)
    (:tessellation-control :tess-control-shader)
    (:tessellation-evaluation :tess-evaluation-shader)
    (:geometry :geometry-shader)
    (:fragment :fragment-shader)
    (:compute :compute-shader)))

(defun make-stage (primitive stage-spec)
  (destructuring-bind (stage-type (&key (version 330)) func-spec) stage-spec
    (let ((func (find-gpu-function func-spec)))
      (varjo:make-stage
       stage-type
       (varjo.internals:in-args func)
       (varjo.internals:uniforms func)
       `(,(ensure-keyword version))
       (varjo.internals:code func)
       t
       (when (eq stage-type :vertex)
         (varjo.internals:primitive-name-to-instance primitive))))))

(defun translate-stages (primitive stage-specs)
  (varjo:rolling-translate
   (mapcar
    (lambda (x) (make-stage primitive x))
    stage-specs)))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (gethash (stage-type stage) (source program))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))


(defmacro defun-gpu (name args &body body)
  (destructuring-bind (in-args uniforms context)
      (varjo.utils:split-arguments args '(&uniform &context))
    `(progn
       (update-gpu-func ',name ',in-args ',uniforms ',context ',body)
       ',name)))

(defun update-gpu-func (name in-args uniforms context body)
  "Use varjo to compile the code. If it succeeds then update the function code
   in varjo and trigger the recompilation hook."
  (varjo.internals::test-translate-function-split-details
   name in-args uniforms context body varjo:*stage-names* t)
  (varjo.internals:add-external-function name in-args uniforms body)
  (funcall *recompilation-hook* (list name (mapcar #'second in-args))))

(setf (macro-function 'defstruct-gpu) (macro-function 'varjo:v-defstruct))
