#lang racket
;;;----------------------------------PART ONE------------------------------------------------------------------
(require csc151www)
(require csc151/rex)
(require csc151)
(require rackunit)


;;;EXTRACTING ALL THE CONTENTS INSIDE THE HTML FILE

;;;(contents-extracted filename)-> list?
;;;filename-> string? w/ extension .htm or .html
;;; Returns a list of all the words in the given file
(define contents-extracted
  (lambda (filename)
    (let* ([xml-code (file->xml filename)]
           [content (apply string-append (sxpath-match "//text()" xml-code))]
           [rex-word (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                             (rex-char-range #\A #\Z)
                                             (rex-string "'")))])
      (rex-find-matches rex-word content))))

;;;CALCULATING DALE C HALL SCORES FROM THE HTML CONTENTS

;;;(total-words filename)-> integer?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Returns the total number of words in (extracted-words filename)
(define total-words
  (lambda (filename)
    (length (contents-extracted filename))))


;;;(num-sentences filename)->integer?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Returns the number of sentences in the file input, <filename>
(define num-sentences
  (lambda (filename)
    (let* ([xml-code (file->xml filename)]
           [content (apply string-append (sxpath-match "//text()" xml-code))]
           [ends/w-fullstop (length (string-split content "."))]
           [ends/w-! (length (string-split content "!"))])
      (+ ends/w-fullstop ends/w-!))))

;;;easy-words->list?
;;;Returns a list of all the easy words as specified by the Dale C Hall method
(define easy-words
  (let ([rex-word
         (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                 (rex-char-range #\A #\Z)
                                 (rex-string "'")))])

    (rex-find-matches rex-word (file->string "Easy.txt"))))


(require csc151)
;;;(element? str)-> list? (of booleans)
;;;str->string?
;;;Returns #t for all the words that are in the Dale C Hall easy word list
(define element?
  (lambda (str)
    (member? str (map string-downcase easy-words))))


;;;(num-difficult-words filename)->integer?
;;;filename-> string? /w extension .htm or .html
;;;Retunrs the number of difficult words in the file input after cross referencing with the lits of easy words provided by the Dale C Hall model
(define num-difficult-words
  (lambda (filename)
    (- (length (contents-extracted filename)) (count identity (map element? (map string-downcase (contents-extracted filename)))))))


;;;(PDW filename)->integer?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Returns the ratio of difficult words to the total words- The Percentage of difficult words
(define PDW
  (lambda (filename)
    (/ (num-difficult-words filename) (total-words filename))))


;;;(ASL filename)->number?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Returns the ratio of the total words to the number of sentences- The average sentence length
(define ASL
  (lambda (filename)
    (/ (total-words filename) (num-sentences filename))))

;;;(PDW>5? filename)->boolean?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;Returns true if the value of (PDW filename)> 5%
(define PDW>5?
  (lambda (filename)
    (> (* 100 (/ (num-difficult-words filename) (total-words filename))) 5)))

;;;(compute-dale-chall-score filename)->number?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Retunrs the value of the Dale C Hall score
(define compute-dale-chall-score
  (lambda (filename)
    (if (PDW>5? filename)
        (+ (+ (* 0.1579 (PDW filename) 100) (* 0.0496 (ASL filename))) 3.6365)
        (+ (* 0.1579 (PDW filename) 100) (* 0.0496 (ASL filename))))))

;;;(score->grade filename)->string?
;;;filename-> string? (The file muts be written in the format "exapmple.txt"
;;;Returns the Dale C Hall grade level of the text input
(define score->grade
  (lambda (filename)
    (cond
      ((< (compute-dale-chall-score filename) 5)
       "4th grade or lower")
      ((and (< (compute-dale-chall-score filename) 6) (>= (compute-dale-chall-score filename) 5))
       "5th-6th grade")
      ((and (< (compute-dale-chall-score filename) 7) (>= (compute-dale-chall-score filename) 6))
       "7th-8th grade")
      ((and (< (compute-dale-chall-score filename) 8) (>= (compute-dale-chall-score filename) 7))
       "9th-10th grade")
      ((and (< (compute-dale-chall-score filename) 9) (>= (compute-dale-chall-score filename) 8))
       "11th-12th grade")
      ((>= (compute-dale-chall-score filename) 9)
       "13th-15th grade"))))

;;;SAVING THE INFORMATION IN AN HTML FILE

;;;(html-base-markup filename)->string? (HTML text)
;;;filename: string? w/ .htm or .html extension
;;;Makes a string for the base code of the HHTML file to be written and created by (add-dc-info infile outfile)
(define html-base-markup
  (let* ([str->num (lambda (n) (number->string n))])
    (lambda (filename)
      (string-append "<em>Total words: </em>" (str->num (total-words filename)) "( Difficult words:  " (str->num(num-difficult-words filename)) "<br>"
                     "<em>Number of sentences: </em>" (str->num (num-sentences filename)) "<br>"
                     "<strong> Given a score of " (str->num (compute-dale-chall-score filename)) ", this document is approbriate for <b> " (score->grade filename) " level of audience. </b></strong>"))))

;;;(extract-html-body infile)->list?
;;;infile: string? (w/ .html OR .htm file extension)
;;;Extracts the body part of the HTML file, given from infile
(define extract-html-body
  (lambda (infile)
    (let* ([html-code (file->string infile)]
           [body-rex (rex-concat (rex-string "<body>")
                                 (rex-repeat-0 (rex-any-char))
                                 (rex-string "</body>"))]
           )
      (rex-find-matches body-rex html-code))))


;;;(add-dc-info infile outfile)->void?
;;;infile: string? w/ a .htm or .html extension
;;;outfile string> w/ an .htm or .html extension
;;;Takes in the content of an input file, infile, calculates the Dale C hall score and then saves it into a new file, named outfile
(define add-dc-info
      (lambda (infile outfile)
      (string->file (string-append "<!DOCTYPE html><html><head> CONTENTS</head>"
                                   (html-base-markup infile)              
                                   (string-join (extract-html-body infile))
                                   "</body></html>")
                     outfile)))


;;;-----------------------------------------PART TWO------------------------------------

(require csc151www)
(require csc151/rex)
(require csc151)
(require rackunit)
(require csc151www)
;;;(list-of-headers infile)->list?
;;;infile: string? (w/ .html OR .htm file extension)
;;;Returns a list of all the headers in the HTML Code
(define list-of-headers
  (lambda (infile)
    (let* ([xml-code (string->xml (file->string infile))]
           [h1 (sxpath-match "//h1" xml-code)]
           [h2 (sxpath-match "//h2" xml-code)]
           [h3 (sxpath-match "//h3" xml-code)]
           [h4 (sxpath-match "//h4" xml-code)]
           [h5 (sxpath-match "//h5" xml-code)]
           [h6 (sxpath-match "//h6" xml-code)]
           [last-elt (lambda (lst) (car (reverse lst)))]

           )
      (flatten (list (map last-elt h1)
                     (map last-elt h2)
                     (map last-elt h3)
                     (map last-elt h4)
                     (map last-elt h5)
                     (map last-elt h6))))))

;;;(make-list-of-contents lst)->list?
;;;lst: list?
;;;Returns a list of the headers, inside the <a href> tags
(define make-list-of-contents
  (lambda (lst)
    (if (null? lst)
        null
        (cons (string-append "<a href=https://www.youtube.com/>" (car lst) "</a><br>") (make-list-of-contents (cdr lst))))))
;;;(contents infile)->string?
;;;infile: string? (w/ .html OR .htm file extension)
;;;Returns a string in the HTML format that has the names of 
(define contents
  (lambda (infile)
    (let* ([headers (list-of-headers infile)]
           [list-of-contents (make-list-of-contents headers)]

           )
      (string-append "<div class=toc>" (string-join list-of-contents) "</div>"))))


;;;(add-toc infile outfile)->file?
;;;infile:  string? (w/ .html OR .htm file extension)
;;;outfile: string? (w/ .html OR .htm file extension) 
;;;Adds a table of contents from the infile to outfile
(define add-toc
  (lambda (infile outfile)
    (string->file (string-append "<!DOCTYPE html><html><head> CONTENTS</head>"
                                 (contents infile)              
                                 (string-join (extract-html-body infile))
                                 "</body></html>")
                  outfile)))
               
    
;;;;---------------------------------------PART THREE------------------------------------


(require csc151www)

;;;Accessibility testing with Image alternative tag

;;;(num-img-codes url)->integer?
;;;url: valid url w/ html webpage?
;;;Returns the number of <img...> codes on the HTML file of the given webpage of url
(define num-img-codes
  (lambda (url)
    (let* ([xml-code (fetch-page url)]
           [w/img-attribute (sxpath-match "//img" xml-code)])
      (length w/img-attribute))))


;;;(num-img-codes/w-alt url)->integer?
;;;url: valid url w/html webpage?
;;;Returns the number of <img..> tags which doesnt have the alt attribute
(define num-invalid-img-codes
  (lambda (url)
    (let* ([xml-code (fetch-page url)]
           [w/alt-attribute (sxpath-match "//img[@alt]" xml-code)])
      (- (num-img-codes url) (length w/alt-attribute)))))


(require csc151)
;;;Accessibility testing with meaningfulness of URL identifiers


;;;(list-of-ahref-tags url)->listof-string?
;;;url: string? (valid URL from the internet)
;;;Returns a list of all the <a href> tags from the html file of the URL
(define list-of-ahref-tags
  (lambda (url)
    (let* ([xml-code (fetch-page url)])
      (sxpath-match "//a[@href]" xml-code))))



;;;(list-of-url-identifiers url)->boolean?
;;;url: string? (valid URL from the internet)
;;;Gives a list of URL identifiers from the <a href> tags
(define list-of-url-identifiers
  (lambda (url)
    (let* ([last-elt (lambda (lst) (car (reverse lst)))]
           [xml-code (fetch-page url)]
           [ahref-tags (list-of-ahref-tags url)]
           )
      (map last-elt ahref-tags))))

(require csc151/rex)

;;;url-rex
;;;rex-expression for sorting out whether identifiers are still in URL format or not
(define url-rex
  (rex-concat (rex-any-of (rex-string "https")
                          (rex-string "http"))
              (rex-repeat (rex-any-char))))

;;;(num-click-here url)->integer?
;;;url: string? (valid URL from the internet)
;;Counts the  number of Url identifiers that have "Click here" written
(define num-click-here
  (lambda(url)
    (let* ([xml-code (fetch-page url)]
           [click-here? (lambda (str) (equal? (string-downcase str) "click here"))]
           )
      (count identity (map click-here? (list-of-url-identifiers url))))))



;;;(number-of-invalid-url-identifiers url)->integer?
;;;url: string? (valid URL from the internet)
;;;Gives the number of invalid URL identifiers (identifiers that are not meaningful)
(define number-of-invalid-url-identifiers
  (lambda (url)
    (let* ([xml-code (fetch-page url)]
           [find-matches (lambda (str) (rex-matches? url-rex str))])
      (+ (num-click-here url) (count identity (map find-matches (list-of-url-identifiers url)))))))


;;;Accessibility testing with testing spaces between links

;;;(space-between-links url)->integer?
;;;url: string? (valid URL from the internet)
;;Rteunrs the number of links that have nothing in between them
(define num-no-space-between-links
  (lambda (url)
    (let* ([rex-word (rex-concat (rex-string "</a>")
                                 (rex-string "<a href="))]
           [xml-code (fetch-page url)]
           [html-code (xml->string xml-code)]
           )
      (count identity (rex-find-matches rex-word html-code)))))

;;;(num-invalid-links url)->integer?
;;;url: string? (valid URL from the internet)
;;;Returns the number of invalid internal links in the website (those that dont have meaning ful text, and no space in between
(define num-invalid-links
  (lambda (url)
    (+ (num-no-space-between-links url) (number-of-invalid-url-identifiers url))))


#|(define sxml
  (lambda (url)
    '(html (body (h1 "Number of Images without alternative texts") (num-invalid-img-codes url) (br)
                 (h1 "Number of Internal Links without Meaningful text" (number-of-invalid-url-identifiers url)) (br)
                 (h1 "Number of Internal links with no space in between" (num-invalid-links url)) (br)))))|#

;;;(assess-accessibility url)->listof sxml?
;;;url: Valid url from the URL with HTML source code
;;;makes an assessment of the webpage in terms of Accessibility and then returns an SXML structure with a count of
;;;all the accessibility issues in the URL
(define assess-accessibility
  (lambda (url)
    (append (list 'html
                  (list 'body
                        (list 'h1 "Number of images without alternative texts")
                        (num-invalid-img-codes url)
                        (list 'br) 
                        (list 'h1 "Number of Internal Links without Meaningful text")
                        (number-of-invalid-url-identifiers url)
                        (list 'br)
                        (list 'h1 "Number of Internal links with no space in between")
                        (num-no-space-between-links url)
                        (list 'h1 "Number of Internal Links with either no Spaces in btween two links, or number of links without meaningful text")
                        (num-invalid-links url)
                        (list 'br))))))

#|EXAMPLES
> (assess-accessibility "https://jimenezp.cs.grinnell.edu/Courses/CSC151/2021Fa/assignments/mp07.html")
'(html
  (body
   (h1 "Number of images without alternative texts")
   0
   (br)
   (h1 "Number of Internal Links without Meaningful text")
   1
   (br)
   (h1 "Number of Internal links with no space in between")
   1
   (br)))


I hosted the following HTML file from the comments into a temporary hosting service, as "https://khanalsa.htmlsave.net/" and then used
it to get the following results

THE HTML FILE:
<!DOCTYPE html>
<html>
<body>

    <h1>My First Heading</h1>
    <p>My first paragraph.</p>
    <img />
    <img />
    <img alt="Non trivial alt that is useful" /> <br>
    <a href="#">Link the First</a><a href="#">Link</a><br>
    <a href="#">Link the third</a> Word in between <a href="#">click here</a><br>
    <a href="#">Meaning Full link shoudl be counted as meaningful</a>
    <a href="#" alt="Some link stuff">Meaning Full link shoudl be counted as meaningful as well</a>
</body>
</html

> (assess-accessibility "https://khanalsa.htmlsave.net/")
'(html
  (body
   (h1 "Number of images without alternative texts")
   2
   (br)
   (h1 "Number of Internal Links without Meaningful text")
   1
   (br)
   (h1 "Number of Internal links with no space in between")
   2
   (br)))



|#

;;;-------------------------------------------------------PART FOUR----------------------------------------------------------------------------

;;;(middle-slobovia-jargon->hash-table?
;;;Table of the replacements for the middle Slobovia dialect 
(define middle-slobovia-jargon (make-hash))
(hash-set! middle-slobovia-jargon "president" "supreme Ruler")
(hash-set! middle-slobovia-jargon "biden" "bye Done")
(hash-set! middle-slobovia-jargon "vaccine" "tracing serum")
(hash-set! middle-slobovia-jargon "virus" "myth")
(hash-set! middle-slobovia-jargon "progressive" "anti-american")
(hash-set! middle-slobovia-jargon "liberal" "marxist")
(hash-set! middle-slobovia-jargon "republican" "authoriatarian")
(hash-set! middle-slobovia-jargon"democrat" "traitor")
(hash-set! middle-slobovia-jargon "scientist" "liar")
(hash-set! middle-slobovia-jargon "science" "lies")
(hash-set! middle-slobovia-jargon "college" "cult")
(hash-set! middle-slobovia-jargon "professor" "indoctrinator")
(hash-set! middle-slobovia-jargon "student" "cultist")
(hash-set! middle-slobovia-jargon "midwest" "heartland")
(require csc151)
(require csc151/rex)



;;;(string->list str)->list?
;;;str: string?
;;;Takes in a string and converts all the words into a list of strings
(define string->list
  (lambda (str)
    (let* ([rex-word (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                             (rex-char-range #\A #\Z)
                                             (rex-string "'")))])
      (rex-find-matches rex-word str))))


;;;(replace/kernel lst)-> void?
;;;lst: list?
;;;Replaces values that have a substritute in the middle Slovobian dialect 
(define replace/kernel
  (lambda (lst)
    (if (null? lst)
        null
        (if (hash-has-key? middle-slobovia-jargon (string-downcase (car lst)))
            (cons (hash-ref middle-slobovia-jargon (string-downcase (car lst))) (replace/kernel (cdr lst)))
            (cons (car lst) (replace/kernel (cdr lst)))))))

;;;(append/w-space str)->void?
;;;str: string?
;;;Appends all the strings of the list into a single string with a space in between
(define append/w-space
  (lambda (str)
    (string-append str " ")))

;;;(replace-xml-text str)->string?
;;;str: string?
;;;Takes in a string and replcaes strings that have a substitute in the middle slobovian dialect with the substitute
(define replace-xml-text
  (lambda (str)
    (apply string-append (map append/w-space (replace/kernel (string->list str))))))


;;;(slobovicize/kernel val) & (slobovicize val)->sxml?
;;;val: sxml?
;;;Slobovicizes the given sxml structure
(define slobovicize/kernel
  (lambda (val)
    (cond
      [(string? val)
       (replace-xml-text val)]
      [else
       val])))


(define slobovicize
  (lambda (val)
    (cond
      [(string? val)
       (slobovicize/kernel val)]
      [(pair? val)
       (cond
         [(eq? (car val) '@)
          val]
         [else
          (map slobovicize val)])]
      [else
       val])))

;;;EXAMPLES FOR (solobovicize sxml)

#|
> (slobovicize press-release)
'(div
  (@ (class "release"))
  (ul
   (li "Dateline " (span (@ (class "date")) ""))
   (li "Location " (span (@ (class "college") "Grinnell"))))
  (p
   "This week supreme Ruler Anne Harris of "
   (span (@class "cult ") "Grinnell cult ")
   "a heartland marxist arts cult known for its anti-american values "
   "announced today that all students must receive the tracing serum "
   "for the corona myth ")
  (p
   "indoctrinator and noted liar Lindsey Smith commenting "
   "on supreme Ruler Harris' announcement said "
   (q "We want to keep our students alive to vote for " "Democrats in the next election "))
  (p
   "In response to supreme Ruler Harris' announcement the authoriatarian "
   "Iowa Legislature voted to require "
   (span (@ (class "medicine")) "horse parasite tablets ")
   "of all students at state schools at the urging of "
   "Senator Iam Old who cited the clear lies for "
   "such medicine "))

> (slobovicize news-article)
'(div
  (@ (class "release"))
  (ul
   (li "Dateline " (span (@ (class "date")) ""))
   (li "Location " (span (@ (class "News") "Reuters"))))
  (p
   "The Democratic controlled U S Senate on Wednesday approved a authoriatarian measure that would overturn supreme Ruler Joe Biden's COVID tracing serum or test mandate for private businesses with two Democrats joining Republicans to back the initiative "
   (span (@class "Ad ") "Washington Post ")
   "supreme Ruler bye Done accounced that the new measures would be beneficial to the public health "
   "The demoannounced today that all students must receive the tracing serum "
   "for the corona myth "))
|#

(define press-release
  '(div (@ (class "release"))
        (ul
         (li "Dateline " (span (@ (class "date")) "2021-11-01"))
         (li "Location " (span (@ (class "college") "Grinnell"))))
        (p "This week, President Anne Harris of "
           (span (@class "college") "Grinnell College")
           ", a midwest liberal-arts college known for its progressive values,"
           " announced today that all students must receive the vaccine"
           " for the corona virus.")
        (p "Professor and noted scientist Lindsey Smith, commenting"
           " on President Harris' announcement said, "
           (q "We want to keep our students alive to vote for"
              " Democrats in the next election."))
        (p "In response to President Harris' announcement, the Republican"
           " Iowa Legislature voted to require "
           (span (@ (class "medicine")) "horse parasite tablets")
           " of all students at state schools, at the urging of"
           " Senator Iam Old, who cited the clear science for"
           " such medicine.")))

(define news-article
  '(div (@ (class "release"))
        (ul
         (li "Dateline " (span (@ (class "date")) "2021-11-01"))
         (li "Location " (span (@ (class "News") "Reuters"))))
        (p "The Democratic-controlled U.S. Senate on Wednesday approved a Republican measure that would
overturn President Joe Biden's COVID-19 vaccine or test mandate for private businesses, with two Democrats
 joining Republicans to back the initiative. "
           (span (@class "Ad") "Washington Post")
           "President Biden accounced that the new measures would be beneficial to the public health"
           " The demoannounced today that all students must receive the vaccine"
           " for the corona virus.")))

;;;;_---------------------------------------------------------------------PART 5

;;;;;;;;;;;;;;(html-text-syllax url) takes a URL with an html page as an input, extracts the text from this HTML page, and then builds a list of lists with one syllable
;;;;;;;;;;;;;; three syllable, upto seven syllable words. (CITATION: IDEA AND FUCNTION EXTRACTED FROM MP5, tweaked it a bit)

;;;(split-to-list word)->list?
;;;word->string?
;;;Splits a string to a list of its letters
(define split-to-list
  (lambda (word)
    (let ([rex-split
           (rex-concat (rex-char-set "abcdefghijklmnopqrstuvwxyz"))])
      (rex-find-matches rex-split word))))


;;;(count-syllables lst)->integer?
;;;lst->list?
;;;Mutually Recursive function for counting the number of syllables, along with (skip-vowels word) 
(define count-syllables
  (lambda (lst)
    (cond
      ((null? lst) 0)
      ((member (car lst) '("a" "e" "i" "o" "u"))
       (+ 1 (skip-vowels (cdr lst))))
      (else
       (count-syllables (cdr lst))))))

;;;(skkip-vowels lst)->integer?
;;;lst->list?
;;;Mutually Recursive function for counting the number of syllables, along with (count-syllables word)
(define skip-vowels
  (lambda (lst)
    (cond
      ((null? lst)
       (count-syllables '()))
      ((member (car lst) '("a" "e" "i" "o" "u"))
       (skip-vowels (cdr lst)))
      (else
       (count-syllables lst)))))


;;;(syllables word)->integer?
;;;word->string?
;;;Returns the number of syllables in a given word
(define syllables
  (lambda (word)
    (count-syllables (split-to-list word))))


;;;(word-list-html-page)->list?
;;;url: string, url? html?
;;;Takes in a url with an html extension, extracts all the words from the contents of that HTML file and builds a list
(define word-list-html-page
  (lambda (url)
    (let ([text (string-join (flatten (sxpath-match "//text()" (fetch-page url))))]
          [rex-word
           (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                   (rex-char-range #\A #\Z)
                                   (rex-string "'")))])
      (map string-downcase (rex-find-matches rex-word text)))))

;;;html-text-syllax->list of lists?
;;;Returns the syllax of the HTML, each list within the list has a certain number of syllables,from one to seven
(define html-text-syllax
  (lambda (url)
    (let ([one-syllable? (lambda (word) (equal? (syllables word) 1))]
          [two-syllable? (lambda (word) (equal? (syllables word) 2))]
          [three-syllable? (lambda (word) (equal? (syllables word) 3))]
          [four-syllable? (lambda (word) (equal? (syllables word) 4))]
          [five-syllable? (lambda (word) (equal? (syllables word) 5))]
          [six-syllable? (lambda (word) (equal? (syllables word) 6))]
          [seven-syllable? (lambda (word) (equal? (syllables word) 7))])
      (list
       ;1
       (filter one-syllable? (word-list-html-page url))
       ;2
       (filter two-syllable? (word-list-html-page url))
       ;3
       (filter three-syllable? (word-list-html-page url))
       ;4
       (filter four-syllable? (word-list-html-page url))
       ;5
       (filter five-syllable? (word-list-html-page url))
       ;6
       (filter six-syllable? (word-list-html-page url))
       ;7
       (filter seven-syllable? (word-list-html-page url))))))



;;;;EXAMPLE FOR PART 5, Please uncomment the following code to run
;(html-text-syllax "https://jimenezp.cs.grinnell.edu/Courses/CSC151/2021Fa/assignments/mp07.html")
;(html-text-syllax "http://www.pratikkarki.com/")
