(set 'arbol  '(1 . ((2 . ((4 . nil) . (5 . nil))) . (3 . ((6 . nil) . (7 . nil))))))

;(set 'arbol2 '(1  ((2  ((4  (nil))) (5 (nil))))) (3  ((6  (nil)) (7  (nil))))) 

(set 'arbol3 '(1 (2 (nil) (4)) (3 (nil))))

;Aqui declaras tu arbol :D
(set 'arbol3 '(1 (2 (4 (nil)) (5 (nil))) (3 (6 (nil)) (7 (nil)) )))

;Recorrido en inorden
(defun inorden (arbolito)
        (if(null (first arbolito)) nil
          (append (inorden (second arbolito)) (cons (first arbolito) nil) (inorden (third arbolito)))))
;Recorrido en preorden
(defun preorden (arbolito)
        (if(null (first arbolito)) nil
          (append (cons (first arbolito) nil) (preorden (second arbolito)) (preorden (third arbolito)))))
;Recorrido en posorden
(defun postorden (arbolito)
        (if(null (first arbolito)) nil
          (append (postorden (second arbolito)) (postorden (third arbolito))  (cons (first arbolito) nil) )))
 
;Funcion Principal
(defun principal (arbol)
    (format nil "Preorden -> ~a ~% Inorden  -> ~a ~% Posorden -> ~a" 
            (preorden   arbol)
            (inorden    arbol)
            (postorden  arbol)))


;Mando a llamar la función principal
(print (principal arbol3))
 
