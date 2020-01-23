#lang racket

(define (derive f)
  (if (eq? (first f) '+)
      (derive+ (first (rest f)) (first (rest (rest f))))
      (if (eq? (first f) '-)
          (derive- (first (rest f)) (first (rest (rest f))))
          (if (eq? (first f) '*)
              (derive* (first (rest f)) (first (rest (rest f))))
              (if (eq? (first f) '/)
                  (derive/ (first (rest f)) (first (rest (rest f))))
                  (derive-u f))))))

(define (derive+ u v)
  (if (list? u)
      (if (list? v)
          (list '+ (derive u)(derive v))
          (if (= (derive-u v) 0)
              (derive u)
              (list '+ (derive u) (derive-u v))))
      (if (list? v)
          (if (= (derive-u u) 0)
              (derive v)
              (list '+ (derive-u u) (derive v)))
          (derive+1 (derive-u u)(derive-u v)))))

(define (derive+1 u v)   ;Simplifications : supprime les 0
  (if (= u 0)
      v
      (if (= v 0)
          u
          (list '+ u v))))


(define (derive- u v)
  (if (list? u)
      (if (list? v)
          (list '- (derive u)(derive v))
          (if (= (derive-u v) 0)
              (derive u)
              (list '- (derive u) (derive-u v))))
      (if (list? v)
          (if (= (derive-u u) 0)
              (list '- (derive v))
              (list '- (derive-u u) (derive v)))
          (derive-1 (derive-u u)(derive-u v)))))

(define (derive-1 u v)   ;Simplifications : supprime les 0
  (if (= u 0)
      (list '- v)
      (if (= v 0)
          u
          (list '- u v))))
  

(define (derive* u v) 
  (if (list? u)
      (if (list? v)
          (list '+ (list '* (derive u) v)(list '* u (derive v)))
          (list '+ (list '* (derive u) v)(list '* u (derive-u v))))
      (if (list? v)
          (list '+ (list '* (derive-u u) v)(list '* u (derive v)))
          (derive*1 u v))))

(define (derive*1 u v)   ;Simplifications : renvoie u'v si v' = 0 OU uv' si u' = 0
  (if (= 0 (derive-u u))
      (derive*2 (list '* u (derive-u v)))
      (if (= 0 (derive-u v))
          (derive*2 (list '* (derive-u u) v))
          (if (eq? u v)   ;renvoie 2 * u si u et v sont égaux et leurs dérivées = 1
              (if (= 1 (derive-u u))
                  (list '* 2 u)
                  (list '+ (list '* (derive-u u) v)(list '* u (derive-u v))))
              (if (and (= 1 (derive-u u)) (= 1 (derive-u v)))
                  (list '+ u v)
                  (list '+ (list '* (derive-u u) v)(list '* u (derive-u v))))))))

(define (derive*2 d)   ;Simplifications : renvoie le paramètre != 1 si l'un des 2 est égal à 1
  (if (= 1 (first (rest  d)))
      (first (rest (rest d)))
      (if (= 1 (first (rest (rest d))))
          (first (rest d))
          d)))


(define (derive/ u v)
  (if (list? u)
      (if (list? v)
          (list '/ (list '- (list '* (derive u) v)(list '* u (derive v)))(list 'expt v 2))
          (list '/ (list '- (list '* (derive u) v)(list '* u (derive-u v)))(list 'expt v 2)))
      (if (list? v)
          (list '/ (list '- (list '* (derive-u u) v)(list '* u (derive v)))(list 'expt v 2))
          (list '/ (list '- (list '* (derive-u u) v)(list '* u (derive-u v)))(list 'expt v 2)))))



(define (derive-u a)   ;Dérivées usuelles
  (if (number? a)
      0
      (if (symbol? a)
          1
          (if (eq? (first a) 'expt) ;à modifier
              (if (and (number? (first (rest (rest a))))(number? (first (rest a))))
                  0
                  (list '* (first (rest (rest a))) (list '* (derive (rest a)) (list 'expt (first (rest a)) (list '- (first (rest (rest a))) 1)))))
              (if (eq? (first a) 'sqrt) ;à modifier
                  (if (number? (first (rest a)))
                      0
                      (list '/ (derive (first (rest a))) (list '* 2 (list 'sqrt (first (rest a))))))
                  (if (eq? (first a) 'cos) ;à modifier
                      (if (number? (first (rest a)))
                          0
                          (list '- 'sin (first (rest a))))
                      (if (eq? (first a) 'sin) ;à modifier
                          (if (number? (first (rest a)))
                              0
                              (list 'cos (first (rest a))))
                          (if (symbol? (first a))
                              1
                              0))))))))
              