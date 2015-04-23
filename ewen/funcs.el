(defun ewen/touchpad-enable (enable)
  (interactive
   (cond ((equal current-prefix-arg nil)
          '(t))
         ((equal current-prefix-arg '(4))
          '(nil))))
  (if enable
      (shell-command "synclient TouchpadOff=0")
      (shell-command "synclient TouchpadOff=1")))


