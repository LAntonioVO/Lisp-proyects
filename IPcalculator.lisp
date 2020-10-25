;############################################################
;FUNCIONES AUXILIARES
(defun obtenerClase (octeto1)
              (if(and (>= octeto1 0) (<= octeto1 127)) "Clase A"
                (if(and (>= octeto1 128) (<= octeto1 191)) "Clase B"
                  (if(and (>= octeto1 192) (<= octeto1 223)) "Clase C"
                  "Error"))))

(defun obtenerNumHosts (mascara)
                   (- (cuadrado 32 mascara) 2))

(defun cuadrado(num1 num2)
                   (expt 2 (- num1 num2)))

(defun decbin (num)
                    (if(= num 0) nil
                      (cons (rem num 2) (decbin (floor num 2)))))

(defun toBin (dec)
                    (reverse (decbin dec)))

(defun sumatoria (oct cont)
                    (if(null oct) 0
                      (+ (* (first oct) (expt 2 cont)) (sumatoria (rest oct) (+ cont 1)))))

(defun toDec (bin)
                    (sumatoria (reverse bin) 0))


(defun andBit (num1 num2)
                     (if(= num1 num2 1) 1
                       0))

(defun octetoAND (oct1 oct2)
                     (if(null oct2) nil
                       (cons (andbit (first oct1) (first oct2)) (octetoAND (rest oct1) (rest oct2)))))


(defun generarCeros (numero)
                   (if(= numero 0) nil
                     (cons 0 (generarCeros(- numero 1)))))

(defun generarUnos (numero)
                   (if(= numero 0) nil
                     (cons 1 (generarUnos(- numero 1)))))

(defun obtNumMascara (prefijo)
                    (append (generarUnos prefijo) (generarCeros (- 32 prefijo))))

(defun acompletarOcteto (oct)
               (if(< (length oct) 8) (append (generarCeros(- 8 (length oct))) oct)
                 oct))

(defun genDirBin(redDecimal)
                    (if(null redDecimal) nil
                        (cons (acompletarOcteto(toBin (first redDecimal))) (genDirBin (rest redDecimal)))))

(defun genDirDec(redBinaria)
                    (if(null redBinaria)nil
                      (cons (toDec (first redBinaria)) (genDirDec (rest redBinaria)))))

(defun mascBin (prefijo)
                    (list (subseq (obtNumMascara prefijo) 0 8 ) (subseq (obtNumMascara prefijo) 8 16) (subseq (obtNumMascara prefijo) 16 24) (subseq (obtNumMascara prefijo) 24 32)))

(defun separarListas (listas)
                    (append (nth 0 listas) (nth 1 listas) (nth 2 listas) (nth 3 listas)))

(defun obtUnosBroad (red prefijo)
                    (append (subseq (separarListas (calcRed (genDirBin red) (mascBin prefijo))) 0 prefijo) (generarUnos (- 32 prefijo))))

(defun obtPrimera (red prefijo)
                    (append (subseq (separarListas (calcRed (genDirBin red) (mascBin prefijo))) 0 prefijo) (generarCeros (- 31 prefijo)) '(1)))

(defun obtUltima (red prefijo)
                    (append (subseq (separarListas (calcRed (genDirBin red) (mascBin prefijo))) 0 prefijo) (generarUnos (- 31 prefijo)) '(0)))


;#########################################
;CALCULAR RED

(defun calcRed(redBin mascaraBin)
                    (if(null redBin)nil
                      (cons (octetoAND (first redBin) (first mascaraBin)) (calcRed (rest redBin) (rest mascaraBin)))))

(defun calcDirRed (red prefijo)
                    (calcRed (genDirBin red) (mascBin prefijo)))


;#########################################
;CALCULAR BROADCAST

(defun calcBroadcast (red prefijo)
                    (list (subseq (obtUnosBroad red prefijo) 0 8 ) (subseq (obtUnosBroad red prefijo) 8 16) (subseq (obtUnosBroad red prefijo) 16 24) (subseq (obtUnosBroad red prefijo) 24 32)))


;#########################################
;CALCULAR PRIMERA
(defun calcPrimera (red prefijo)
                    (list (subseq (obtPrimera red prefijo) 0 8 ) (subseq (obtPrimera red prefijo) 8 16) (subseq (obtPrimera red prefijo) 16 24) (subseq (obtPrimera red prefijo) 24 32)))

;#########################################
;CALCULAR ULTIMA

(defun calcUltima (red prefijo)
                    (list (subseq (obtUltima red prefijo) 0 8 ) (subseq (obtUltima red prefijo) 8 16) (subseq (obtUltima red prefijo) 16 24) (subseq (obtUltima red prefijo) 24 32)))

;#########################################
;FUNCIÓN PRINCIPAL
(defun principal(red prefijo) 
    (format nil "Red:       ~a -> ~a ~% Primera:   ~a -> ~a ~% Ultima:    ~a -> ~a  ~% Broadcast: ~a -> ~a ~% Numero Host: ~a ~% ~a"
                    (genDirDec(calcDirRed    red prefijo)) (calcDirRed    red prefijo)
                    (genDirDec(calcPrimera   red prefijo)) (calcPrimera   red prefijo)
                    (genDirDec(calcUltima    red prefijo)) (calcUltima    red prefijo)
                    (genDirDec(calcBroadcast red prefijo)) (calcBroadcast red prefijo)
                    (obtenerNumHosts prefijo)
                    (obtenerClase (first red))))



(print (principal '(192 168 1 10) 3)) 
