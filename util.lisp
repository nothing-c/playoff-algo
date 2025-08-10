;; Misc utility functions and definitions
;; All games are (winner . loser)
(defvar all-games (list 2014-games
		    2015-games
		    2016-games
		    2017-games
		    2018-games
		    2019-games
		    2021-games
		    2022-games
		    2023-games
		    2024-games))

(defvar *year* 0000 "Global variable to make macros handle year-by-year conference changes")

(defun flatten-and-uniq (x)
  "flatten ( . ) lists and return unique elements"
  (remove-duplicates (reduce #'append (mapcar (lambda (y) (list (car y) (cdr y))) x))))

(defvar unique-by-year (mapcar #'flatten-and-uniq all-games))

(defun get-conf (x y)
  "Get conference of X in year Y"
  (car (remove-if (lambda (z) (eq z '())) (mapcar (lambda (z) (if (contains x (cadr (assoc y (cdr z)))) (car z))) conf-alist))))

;; not accounting for conference elo
;; (defun eloize-gamelist (x y)
;;   `(progn
;;      (setq *year* ,y)
;;      (eloize ,(flatten-and-uniq x) ,@(mapcar (lambda (y) `(beats ,(car y) ,(cdr y))) x))))

;; accounting for conference elo
(defun eloize-gamelist (x y)
  "Will generate a ton of errors due to code gen, but does not actually hurt the final result!"
  `(progn
     (setq *year* ,y)
     (eloize
      ,(append (mapcar #'car conf-alist) (flatten-and-uniq x))
      ,@(mapcar (lambda (z)
		  `(progn
		     (cond
		       (,(not (fbsp (car z) y)) (beats ,(car z) ,(cdr z))) ; needs to exist bc or is a macro & if it passes through then it'll try to call get-conf on an fcs team
		       (,(not (fbsp (cdr z) y)) (beats ,(car z) ,(cdr z)))
		       ((same-conf ,(car z) ,(cdr z) ,y) (beats ,(car z) ,(cdr z)))
		       (t (progn
			    (beats ,(get-conf (car z) y) ,(get-conf (cdr z) y))
			    (beats ,(car z) ,(cdr z))
			    )))
		     )
		  )
		x))))

(defun sort-elos (x)
  (sort x #'(lambda (y z) (> (cdr y) (cdr z)))))

;; Top 16 of every year, without taking conferences into consideration
;; (defvar simple-top-16 (mapcar (lambda (x) (top-16 (sort-elos (eval (eloize-gamelist x))))) all-games))

(defun top-16 (x)
  (loop for i from 0 to 15		; ugly but I don't want to dig through the hyperspec
	collect (nth i x)))

(defun top-16-elo-mean (teams y)
  "Top 16 teams in year y based on (mean elo conf-elo)) from elo list"
  (top-16
   (sort-elos (mapcar
	       (lambda (x) (cons (car x) (/ (+ (cdr x) (cdr (assoc (get-conf (car x) y) teams))) 2)))
	       (remove-if
		(lambda (x) (or (not (fbsp (car x) y)) (contains (car x) (mapcar #'car conf-alist)))) teams)))))

(defun top-16-elo-sum (teams y)
  "Top 16 teams in year y based on (sum elo conf-elo)) from elo list"
  (top-16
   (sort-elos (mapcar
	       (lambda (x) (cons (car x) (+ (cdr x) (cdr (assoc (get-conf (car x) y) teams)))))
	       (remove-if
		(lambda (x) (or (not (fbsp (car x) y)) (contains (car x) (mapcar #'car conf-alist)))) teams)))))

(defun contains (x l)
  "X is in L?"
  (some (lambda (y) (eq x y)) l))

(defun fbsp (x y)
  "Check if team x is FBS for a certain year y"
  (contains t (mapcar (lambda (z) (contains x z)) (mapcar #'cadr (mapcar (lambda (z) (assoc y z)) conferences)))))

(defun same-conf (t1 t2 y)
  "Are the teams in the same conference this year?"
  (contains t (mapcar (lambda (x) (and (contains t1 x) (contains t2 x))) (mapcar (lambda (x) (cadr (assoc y x))) conferences))))

;; non-cons only
;; (remove-if (lambda (x) (same-conf (car x) (cdr x) 2014)) 2014-games)
;; conference games only
;; (remove-if-not (lambda (x) (same-conf (car x) (cdr x) 2014)) 2014-games)
(defun confgames (y)
  "All in-conference games for year y"
  (remove-if-not (lambda (x) (same-conf (car x) (cdr x) y)) (eval (read-from-string (format '() "~s-games" y)))))

(defun conferencep (x)
  "Is X a conference?"
  (contains x conferences))

(defun top-16-all ()
  (list (top-16 (sort-elos (eval (eloize-gamelist 2014-games 2014))))
	(top-16 (sort-elos (eval (eloize-gamelist 2015-games 2015))))
	(top-16 (sort-elos (eval (eloize-gamelist 2016-games 2016))))
	(top-16 (sort-elos (eval (eloize-gamelist 2017-games 2017))))
	(top-16 (sort-elos (eval (eloize-gamelist 2018-games 2018))))
	(top-16 (sort-elos (eval (eloize-gamelist 2019-games 2019))))
	(top-16 (sort-elos (eval (eloize-gamelist 2021-games 2021))))
	(top-16 (sort-elos (eval (eloize-gamelist 2022-games 2022))))
	(top-16 (sort-elos (eval (eloize-gamelist 2023-games 2023))))
	(top-16 (sort-elos (eval (eloize-gamelist 2024-games 2024))))))

(defun top-16-mean-all ()
  (list
   (top-16-elo-mean (eval (eloize-gamelist 2014-games 2014)) 2014)
   (top-16-elo-mean (eval (eloize-gamelist 2015-games 2015)) 2015)
   (top-16-elo-mean (eval (eloize-gamelist 2016-games 2016)) 2016)
   (top-16-elo-mean (eval (eloize-gamelist 2017-games 2017)) 2017)
   (top-16-elo-mean (eval (eloize-gamelist 2018-games 2018)) 2018)
   (top-16-elo-mean (eval (eloize-gamelist 2019-games 2019)) 2019)
   (top-16-elo-mean (eval (eloize-gamelist 2021-games 2021)) 2021)
   (top-16-elo-mean (eval (eloize-gamelist 2022-games 2022)) 2022)
   (top-16-elo-mean (eval (eloize-gamelist 2023-games 2023)) 2023)
   (top-16-elo-mean (eval (eloize-gamelist 2024-games 2024)) 2024)))

(defun top-16-sum-all ()
  (list
   (top-16-elo-sum (eval (eloize-gamelist 2014-games 2014)) 2014)
   (top-16-elo-sum (eval (eloize-gamelist 2015-games 2015)) 2015)
   (top-16-elo-sum (eval (eloize-gamelist 2016-games 2016)) 2016)
   (top-16-elo-sum (eval (eloize-gamelist 2017-games 2017)) 2017)
   (top-16-elo-sum (eval (eloize-gamelist 2018-games 2018)) 2018)
   (top-16-elo-sum (eval (eloize-gamelist 2019-games 2019)) 2019)
   (top-16-elo-sum (eval (eloize-gamelist 2021-games 2021)) 2021)
   (top-16-elo-sum (eval (eloize-gamelist 2022-games 2022)) 2022)
   (top-16-elo-sum (eval (eloize-gamelist 2023-games 2023)) 2023)
   (top-16-elo-sum (eval (eloize-gamelist 2024-games 2024)) 2024)))

(defun prettyprint-rankings (x)
  (mapcar (lambda (y) (mapcar (lambda (z) (format t "~s,~d~%" (car z) (cdr z))) y)) x))

;; (load "conf.lisp")
;; (load "elo.lisp")
;; (load "games.lisp")
;; (load "util.lisp")
