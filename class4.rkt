#lang plai-typed

(define (split [pivot : number] [a : (listof number)] [lsplit : (listof number)] [rsplit : (listof number)]) : (listof (listof number))
  (cond
    [(empty? a) (cons lsplit (cons rsplit (list)))]
    [else (cond
            [(< (first a) pivot) (split pivot (rest a) (cons (first a) lsplit) rsplit)]
            [else (split pivot (rest a) lsplit (cons (first a) rsplit))])]))


(define (qsort [a : (listof number)]) : (listof number)
  (cond
    [(empty? a) a]
    [(equal? (length a) 1) a]
    [else (let* ([pivot (first a)]
                 [asplit (split pivot (rest a) (list) (list))]
                 [lsplit (qsort (first asplit))]
                 [rsplit (qsort (second asplit))])
            (append lsplit (cons pivot rsplit)))]))


(test (qsort (list 1 2 3)) (list 1 2 3))
(test (qsort (list 45 2.34 -4 84 3)) (list -4 2.34 3 45 84))