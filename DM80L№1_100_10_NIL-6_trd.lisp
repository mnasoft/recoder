;;;; DM80L№1_100_10_NIL-6_trd.lisp

(in-package #:recoder)

(progn
  (defparameter *ht-s-o* (make-hash-table :test 'equal))
  (let ((s-o
	 '(("GQ010"    "Ne")
	   ("EN1"      "n<sub>КНД</sub>")
	   ("EN2"      "n<sub>КВД</sub>")
	   ("T04"      "t<sub>04</sub>")

	   ("dT04plus"  "Δt<sub>04max</sub>")
	   ("dT04minus" "Δt<sub>04min</sub>")
           ("T01"       "t<sub>НВ</sub>")
           ("P02"       "p<sub>2</sub>")
	   ("FT020"     "t<sub>ДТ</sub>")
	   ("FP210"     "p<sub>КДТ1</sub>")
	   ("FP220"     "p<sub>КДТ2</sub>")
	   ("FA020"     "K<sub>ДТ</sub>")
	   ("FQ110"     "G<sub>ДТ</sub>")
	   ("FT010"     "t<sub>ГТ</sub>")
	   ("FP230"     "p<sub>КГТ3</sub>")
	   ("FA010"     "K<sub>ГТ</sub>")
	   ("FQ010"     "G<sub>ГТ</sub>")
	   ("PT240"     "t<sub>В_ПР_КДТ1</sub>")
	   ("PT230"     "t<sub>В_ПР_КДТ2</sub>")
	   ("PT250"     "t<sub>В_ПР_КГТ3</sub>")
	   
	   ("EB100"     "П<sub>КС.1</sub>")
	   ("EB110"     "П<sub>КС.2</sub>")
	   ("EB120"     "П<sub>КС.3</sub>")
	   
	   ("TSI_T165" "t<sub>02.1</sub>")
	   ("TSI_T166" "t<sub>02.2</sub>")
	   ("TSI_T167" "t<sub>02.3</sub>")
	   ("TSI_T168" "t<sub>02.4</sub>")
	   ("TSI_T169" "t<sub>02.5</sub>")

	   ("TSI_T000" "t<sub>ЖТ3.1</sub>")
	   ("TSI_T030" "t<sub>ЖТ3.2</sub>")
	   ("TSI_T031" "t<sub>ЖТ3.3</sub>")
	   ("TSI_T032" "t<sub>ЖТ3.4</sub>")
	   ("TSI_T033" "t<sub>ЖТ3.5</sub>")
	   ("TSI_T001" "t<sub>ЖТ3.6</sub>")
	   ("TSI_T002" "t<sub>ЖТ3.7</sub>")
	   ("TSI_T003" "t<sub>ЖТ3.8</sub>")
	   ("TSI_T004" "t<sub>ЖТ3.9</sub>")
	   ("TSI_T035" "t<sub>ЖТ3.10</sub>")
	   ("TSI_T036" "t<sub>ЖТ3.11</sub>")
	   ("TSI_T037" "t<sub>ЖТ3.12</sub>")
	   ("TSI_T038" "t<sub>ЖТ3.13</sub>")
	   ("TSI_T005" "t<sub>ЖТ3.14</sub>")
	   ("TSI_T006" "t<sub>ЖТ3.15</sub>")
	   ("TSI_T007" "t<sub>ЖТ3.16</sub>")
	   ("TSI_T008" "t<sub>ЖТ3.17</sub>")
	   ("TSI_T039" "t<sub>ЖТ3.18</sub>")
	   ("TSI_T040" "t<sub>ЖТ3.19</sub>")
	   ("TSI_T041" "t<sub>ЖТ3.20</sub>")

	   ("TSI_T042" "t<sub>ЖТ3.21</sub>")
	   ("TSI_T043" "t<sub>ЖТ3.21</sub>")

	   ("TSI_T009" "t<sub>ЖТ3.22</sub>")
	   ("TSI_T010" "t<sub>ЖТ3.23</sub>")
	   ("TSI_T011" "t<sub>ЖТ3.24</sub>")
	   ("TSI_T044" "t<sub>ЖТ3.25</sub>")
	   ("TSI_T045" "t<sub>ЖТ3.26</sub>")

	   ("TSI_T047" "t<sub>ЖТ3.28</sub>")
	   ("TSI_T046" "t<sub>ЖТ3.29</sub>")
	   ("TSI_T012" "t<sub>ЖТ3.30</sub>")
	   ("TSI_T013" "t<sub>ЖТ3.31</sub>")
	   ("TSI_T014" "t<sub>ЖТ3.32</sub>")
	   ("TSI_T048" "t<sub>ЖТ3.33</sub>")
	   ("TSI_T049" "t<sub>ЖТ3.34</sub>")
	   ("TSI_T034" "t<sub>ЖТ3.35</sub>")

	   ("TSI_T015" "t<sub>ЖТ3.36</sub>")
	   ("TSI_T050" "t<sub>ЖТ3.36</sub>")

	   ("TSI_T051" "t<sub>ЖТ3.37</sub>")
	   ("TSI_T016" "t<sub>ЖТ3.38</sub>")
	   ("TSI_T017" "t<sub>ЖТ3.39</sub>")
	   ("TSI_T018" "t<sub>ЖТ3.40</sub>")
	   ("TSI_T052" "t<sub>ЖТ3.41</sub>")
	   ("TSI_T053" "t<sub>ЖТ3.42</sub>")
	   ("TSI_T054" "t<sub>ЖТ3.43</sub>")
	   ("TSI_T019" "t<sub>ЖТ3.44</sub>")
	   ("TSI_T055" "t<sub>ЖТ3.45</sub>")
	   ("TSI_T056" "t<sub>ЖТ3.46</sub>")
	   ("TSI_T057" "t<sub>ЖТ3.47</sub>")
	   ("TSI_T020" "t<sub>ЖТ3.48</sub>")
	   ("TSI_T058" "t<sub>ЖТ3.49</sub>")
	   ("TSI_T059" "t<sub>ЖТ3.50</sub>")
	   ("TSI_T060" "t<sub>ЖТ3.51</sub>")
	   ("TSI_T061" "t<sub>ЖТ3.52</sub>")
	   ("TSI_T021" "t<sub>ЖТ3.53</sub>")
	   ("TSI_T022" "t<sub>ЖТ3.54</sub>")
	   ("TSI_T023" "t<sub>ЖТ3.55</sub>")
	   ("TSI_T062" "t<sub>ЖТ3.56</sub>")
	   ("TSI_T065" "t<sub>ЖТ3.57</sub>")
	   ("TSI_T064" "t<sub>ЖТ3.58</sub>")
	   ("TSI_T063" "t<sub>ЖТ3.59</sub>")
	   ("TSI_T067" "t<sub>ЖТ3.60</sub>")
	   ("TSI_T024" "t<sub>ЖТ3.61</sub>")
	   ("TSI_T025" "t<sub>ЖТ3.62</sub>")
	   ("TSI_T026" "t<sub>ЖТ3.63</sub>")
	   ("TSI_T068" "t<sub>ЖТ3.64</sub>")
	   ("TSI_T069" "t<sub>ЖТ3.65</sub>")
	   ("TSI_T066" "t<sub>ЖТ3.66</sub>")
	   ("TSI_T070" "t<sub>ЖТ3.67</sub>")
	   ("TSI_T071" "t<sub>ЖТ3.68</sub>")

	   ("TSI_T027" "t<sub>ЖТ3.70</sub>")
	   ("TSI_T028" "t<sub>ЖТ3.71</sub>")
	   ("TSI_T072" "t<sub>ЖТ3.72</sub>")
	   ("TSI_T073" "t<sub>ЖТ3.73</sub>")
	   ("TSI_T074" "t<sub>ЖТ3.74</sub>")
	   ("TSI_T029" "t<sub>ЖТ3.75</sub>")
	   ("TSI_T075" "t<sub>ЖТ3.76</sub>")


	   ("TSI_T111" "t<sub>ЖТ15.1</sub>")
	   ("TSI_T112" "t<sub>ЖТ15.2</sub>")
	   ("TSI_T113" "t<sub>ЖТ15.3</sub>")
	   ("TSI_T114" "t<sub>ЖТ15.4</sub>")
	   ("TSI_T080" "t<sub>ЖТ15.5</sub>")
	   ("TSI_T081" "t<sub>ЖТ15.6</sub>")
	   ("TSI_T082" "t<sub>ЖТ15.7</sub>")
	   ("TSI_T115" "t<sub>ЖТ15.8</sub>")
	   ("TSI_T116" "t<sub>ЖТ15.9</sub>")
	   ("TSI_T117" "t<sub>ЖТ15.10</sub>")
	   ("TSI_T118" "t<sub>ЖТ15.11</sub>")
	   ("TSI_T119" "t<sub>ЖТ15.12</sub>")
	   ("TSI_T083" "t<sub>ЖТ15.13</sub>")
	   ("TSI_T084" "t<sub>ЖТ15.14</sub>")
	   ("TSI_T085" "t<sub>ЖТ15.15</sub>")

	   ("TSI_T086" "t<sub>ЖТ15.16</sub>")
	   ("TSI_T120" "t<sub>ЖТ15.16</sub>")

	   ("TSI_T121" "t<sub>ЖТ15.17</sub>")
	   ("TSI_T122" "t<sub>ЖТ15.18</sub>")
	   ("TSI_T123" "t<sub>ЖТ15.19</sub>")
	   ("TSI_T124" "t<sub>ЖТ15.20</sub>")
	   ("TSI_T087" "t<sub>ЖТ15.21</sub>")
	   ("TSI_T088" "t<sub>ЖТ15.22</sub>")
	   ("TSI_T089" "t<sub>ЖТ15.23</sub>")
	   ("TSI_T125" "t<sub>ЖТ15.24</sub>")
	   ("TSI_T126" "t<sub>ЖТ15.25</sub>")
	   ("TSI_T127" "t<sub>ЖТ15.26</sub>")
	   ("TSI_T128" "t<sub>ЖТ15.27</sub>")
	   ("TSI_T129" "t<sub>ЖТ15.28</sub>")
	   ("TSI_T130" "t<sub>ЖТ15.29</sub>")
	   ("TSI_T090" "t<sub>ЖТ15.30</sub>")
	   ("TSI_T091" "t<sub>ЖТ15.31</sub>")
	   ("TSI_T131" "t<sub>ЖТ15.32</sub>")
	   ("TSI_T132" "t<sub>ЖТ15.33</sub>")
	   ("TSI_T133" "t<sub>ЖТ15.34</sub>")
	   ("TSI_T134" "t<sub>ЖТ15.35</sub>")
	   ("TSI_T135" "t<sub>ЖТ15.36</sub>")
	   ("TSI_T092" "t<sub>ЖТ15.37</sub>")
	   ("TSI_T093" "t<sub>ЖТ15.38</sub>")
	   ("TSI_T094" "t<sub>ЖТ15.39</sub>")
	   ("TSI_T136" "t<sub>ЖТ15.40</sub>")
	   ("TSI_T137" "t<sub>ЖТ15.41</sub>")
	   ("TSI_T138" "t<sub>ЖТ15.42</sub>")
	   ("TSI_T095" "t<sub>ЖТ15.43</sub>")

	   ("TSI_T139" "t<sub>ЖТ15.45</sub>")
	   ("TSI_T097" "t<sub>ЖТ15.45</sub>")

	   ("TSI_T140" "t<sub>ЖТ15.46</sub>")
	   ("TSI_T098" "t<sub>ЖТ15.47</sub>")
	   ("TSI_T099" "t<sub>ЖТ15.48</sub>")
	   ("TSI_T141" "t<sub>ЖТ15.49</sub>")
	   ("TSI_T142" "t<sub>ЖТ15.50</sub>")
	   ("TSI_T143" "t<sub>ЖТ15.51</sub>")
	   ("TSI_T144" "t<sub>ЖТ15.52</sub>")

	   ("TSI_T096" "t<sub>ЖТ15.54</sub>")
	   ("TSI_T100" "t<sub>ЖТ15.55</sub>")
	   ("TSI_T145" "t<sub>ЖТ15.56</sub>")
	   ("TSI_T101" "t<sub>ЖТ15.57</sub>")
	   ("TSI_T146" "t<sub>ЖТ15.58</sub>")
	   ("TSI_T147" "t<sub>ЖТ15.59</sub>")
	   ("TSI_T151" "t<sub>ЖТ15.60</sub>")

	   ("TSI_T102" "t<sub>ЖТ15.62</sub>")
	   ("TSI_T152" "t<sub>ЖТ15.63</sub>")
	   ("TSI_T153" "t<sub>ЖТ15.64</sub>")
	   ("TSI_T154" "t<sub>ЖТ15.65</sub>")
	   ("TSI_T155" "t<sub>ЖТ15.66</sub>")

	   ("TSI_T156" "t<sub>ЖТ15.67</sub>")
	   ("TSI_T157" "t<sub>ЖТ15.67</sub>")

	   ("TSI_T158" "t<sub>ЖТ15.68</sub>")
	   ("TSI_T103" "t<sub>ЖТ15.69</sub>")
	   ("TSI_T104" "t<sub>ЖТ15.70</sub>")
	   ("TSI_T159" "t<sub>ЖТ15.71</sub>")
	   ("TSI_T160" "t<sub>ЖТ15.72</sub>")
	   ("TSI_T161" "t<sub>ЖТ15.73</sub>")
	   ("TSI_T162" "t<sub>ЖТ15.74</sub>")
	   ("TSI_T163" "t<sub>ЖТ15.75</sub>")
	   ("TSI_T164" "t<sub>ЖТ15.76</sub>")

	   )))
    (mapc #'(lambda (el) (setf (gethash (first el) *ht-s-o*) (second el)))  s-o)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nis6-tgt-str*
'("GQ010" "EN1" "EN2" "T04" "TSI_T165" "TSI_T166" "TSI_T167" "TSI_T168" "TSI_T169"
"TSI_T000" "TSI_T030" "TSI_T031" "TSI_T032" "TSI_T033" "TSI_T001" "TSI_T002" "TSI_T003"
"TSI_T004" "TSI_T035" "TSI_T036" "TSI_T037" "TSI_T038" "TSI_T005" "TSI_T006" "TSI_T007"
"TSI_T008" "TSI_T039" "TSI_T040" "TSI_T041" "TSI_T042" "TSI_T043" "TSI_T009" "TSI_T010"
"TSI_T011" "TSI_T044" "TSI_T045" "TSI_T047" "TSI_T046" "TSI_T012" "TSI_T013" "TSI_T014"
"TSI_T048" "TSI_T049" "TSI_T034" "TSI_T015" "TSI_T050" "TSI_T051" "TSI_T016" "TSI_T017"
"TSI_T018" "TSI_T052" "TSI_T053" "TSI_T054" "TSI_T019" "TSI_T055" "TSI_T056" "TSI_T057"
"TSI_T020" "TSI_T058" "TSI_T059" "TSI_T060" "TSI_T061" "TSI_T021" "TSI_T022" "TSI_T023"
"TSI_T062" "TSI_T065" "TSI_T064" "TSI_T063" "TSI_T067" "TSI_T024" "TSI_T025" "TSI_T026"
"TSI_T068" "TSI_T069" "TSI_T066" "TSI_T070" "TSI_T071" "TSI_T027" "TSI_T028" "TSI_T072"
"TSI_T073" "TSI_T074" "TSI_T029" "TSI_T075" "TSI_T111" "TSI_T112" "TSI_T113" "TSI_T114"
"TSI_T080" "TSI_T081" "TSI_T082" "TSI_T115" "TSI_T116" "TSI_T117" "TSI_T118" "TSI_T119"
"TSI_T083" "TSI_T084" "TSI_T085" "TSI_T086" "TSI_T120" "TSI_T121" "TSI_T122" "TSI_T123"
"TSI_T124" "TSI_T087" "TSI_T088" "TSI_T089" "TSI_T125" "TSI_T126" "TSI_T127" "TSI_T128"
"TSI_T129" "TSI_T130" "TSI_T090" "TSI_T091" "TSI_T131" "TSI_T132" "TSI_T133" "TSI_T134"
"TSI_T135" "TSI_T092" "TSI_T093" "TSI_T094" "TSI_T136" "TSI_T137" "TSI_T138" "TSI_T095"
"TSI_T139" "TSI_T097" "TSI_T140" "TSI_T098" "TSI_T099" "TSI_T141" "TSI_T142" "TSI_T143"
"TSI_T144" "TSI_T096" "TSI_T100" "TSI_T145" "TSI_T101" "TSI_T146" "TSI_T147" "TSI_T151"
"TSI_T102" "TSI_T152" "TSI_T153" "TSI_T154" "TSI_T155" "TSI_T156" "TSI_T157" "TSI_T158"
"TSI_T103" "TSI_T104" "TSI_T159" "TSI_T160" "TSI_T161" "TSI_T162" "TSI_T163" "TSI_T164"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-trd (trd-fname html-fname str-signal-list time )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-html-trd
 "~/develop/TRD/DM80L№1-100-10/NIL-6/20170123_090853.trd"
 "~/rez-nil6-tgt-20170123_090853.html"
  *nis6-tgt-str*
 (list(encode-universal-time 00 21 9  23 1 2017) ;;;; 1.0 МВт
      (encode-universal-time 07 31 9  23 1 2017) ;;;; 4.0 МВт
      (encode-universal-time 50 42 9  23 1 2017) ;;;; 8.0 МВт
      (encode-universal-time 19 52 9  23 1 2017) ;;;; 11.0 МВт
      (encode-universal-time 02 12 10 23 1 2017) ;;;; 14.0 МВт
      (encode-universal-time 18 33 10 23 1 2017) ;;;; 16.0 МВт
      (encode-universal-time 53 53 10 23 1 2017) ;;;; 18.0 МВт
      (encode-universal-time 53 12 11 23 1 2017) ;;;; 20.8 МВт
      (encode-universal-time 16 33 11 23 1 2017) ;;;; 23.16 МВт
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-html-trd
 "~/develop/TRD/DM80L№1-100-10/NIL-6/20170126_105142.trd"
  "~/rez-nil6-tgt-20170126_105142.html"
  *nis6-tgt-str*
   (list (encode-universal-time 18 19 15 26 1 2017) ;;;; 1.0 МВт
       (encode-universal-time 46 38 12 26 1 2017)   ;;;; 4.0 МВт
       (encode-universal-time 23 04 13 26 1 2017)   ;;;; 8.0 МВт
       (encode-universal-time 24 35 13 26 1 2017)   ;;;; 11.0 МВт
       (encode-universal-time 58 52 13 26 1 2017)   ;;;; 14.0 МВт
       (encode-universal-time 58 14 14 26 1 2017)   ;;;; 16.0 МВт
       (encode-universal-time 46 49 14 26 1 2017)   ;;;; 18.0 МВт
       ))


