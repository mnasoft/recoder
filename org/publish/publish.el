(progn
  ;;(setq n133906-prefix "//n133906/home/_namatv/public_html/Site/CL/")
  ;;(setq fscluster-prefix "//fscluster/KO/Temp/Отд11/CFD/")
  (org-setup "~/public_html/CL" (home-ancestor 2))
;;;;
  (setq org-publish-project-alist
        `(
          ,(org-pub-list ""             "")          
          ,(org-pub-list "org"          "org" :recursive t)
          ,(org-pub-list "pub"          "org/publish")
;;;;          
;;;;      ,(org-att-list "images" "png" "images")
          ,(org-att-list "pub"    "el"  "org/publish")
          ))
  (org-web-list))

(progn
  (require 'ox-publish)
  (setq org-publish-use-timestamps-flag nil)
  (setq org-confirm-babel-evaluate nil) ; выполнение всех блоков кода без подтверждения    
  (org-publish-project "website"))
