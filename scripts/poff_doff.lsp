;; =====================================================================
;; POFF / DOFF — офсеты полилиний с объединением в замкнутый контур
;; =====================================================================
;;
;;  poff / зщаа
;;      Офсет в одну сторону с объединением в полилинию
;;      (бывш. WINDOW / WI)
;;
;;  doff / вщаа
;;      Офсет в обе стороны с объединением в полилинию
;;      (бывш. OFFSETPOLYCLOSE / tube / ТРУБА)
;;
;; =====================================================================

(vl-load-com)

;; =====================================================================
;; ГЛОБАЛЬНЫЕ ФУНКЦИИ ДЛЯ УПРАВЛЕНИЯ СИСТЕМНЫМИ ПЕРЕМЕННЫМИ
;; =====================================================================

(defun save-sysvars (vars-list / saved-vars var)
  (setq saved-vars '())
  (foreach var vars-list
    (if (member var '(OSMODE ORTHOMODE COORDS SNAPMODE GRIDMODE))
      (setq saved-vars (cons (cons var (getvar var)) saved-vars))
      (setq saved-vars (cons (cons var (getvar var)) saved-vars))
    )
  )
  saved-vars
)

(defun restore-sysvars (saved-vars)
  (if saved-vars
    (foreach var saved-vars
      (if (and (car var) (cdr var))
        (setvar (car var) (cdr var))
      )
    )
  )
)

;; =====================================================================
;; POFF — офсет в одну сторону + замкнутый контур
;; =====================================================================
(defun c:poff (/ *error* saved-vars *dist* ent point obj offsetObj offsetEname
                   offsetDist continue answer distInput
                   sourceLayer objectsToDelete resultPoly cmdActive
                   vertices1 vertices2 closedPoly)
  
  (setq saved-vars (save-sysvars '(CMDECHO OSMODE ORTHOMODE COORDS SNAPMODE GRIDMODE PEDITACCEPT)))
  
  (if (not (boundp '*dist*))
    (setq *dist* 70.0)
  )
  
  (defun *error* (msg)
    (restore-sysvars saved-vars)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )
  
  (setq continue T)
  
  (while continue
    (setvar 'CMDECHO 0)
    (setvar 'OSMODE 0)
    (setvar 'PEDITACCEPT 1)
    
    (setq objectsToDelete nil)
    
    (princ (strcat "\nТекущее расстояние: " (rtos *dist*)))
    (setq distInput (getdist (strcat "\nВведите расстояние смещения <" (rtos *dist*) ">: ")))
    (if distInput
      (setq *dist* distInput)
    )
    
    (setq ent nil)
    (while (not ent)
      (setq ent (car (entsel "\nВыберите полилинию или отрезок: ")))
      (if (not ent)
        (princ "\nОбъект не выбран. Повторите выбор.")
      )
    )
    
    (setq objectsToDelete (cons ent objectsToDelete))
    
    (setq sourceLayer (cdr (assoc 8 (entget ent))))
    
    (setq obj (vlax-ename->vla-object ent))
    
    (if (not (member (vla-get-ObjectName obj) '("AcDbPolyline" "AcDbLine" "AcDb2dPolyline")))
      (progn
        (princ "\nВыбран не полилиния и не отрезок.")
        (setq ent nil)
        (setq objectsToDelete nil)
        (restore-sysvars saved-vars)
        (continue)
      )
    )
    
    (setq point nil)
    (while (not point)
      (princ "\nУкажите сторону смещения курсором...")
      (setq point (getpoint "\nУкажите сторону смещения курсором: "))
    )
    
    (setq testDist *dist*)
    (setq offsetObjTest (car (vlax-invoke obj 'Offset testDist)))
    
    (if offsetObjTest
      (progn
        (setq offsetEnameTest (vlax-vla-object->ename offsetObjTest))
        (setq offsetPoint (vlax-curve-getClosestPointTo offsetEnameTest point))
        
        (vla-delete offsetObjTest)
        
        (setq closestPoint (vlax-curve-getClosestPointTo obj point))
        (setq distToOriginal (distance point closestPoint))
        (setq distToOffset (distance point offsetPoint))
        
        (if (< distToOffset distToOriginal)
          (setq offsetDist *dist*)
          (setq offsetDist (- *dist*))
        )
      )
      (progn
        (setq testDist (- *dist*))
        (setq offsetObjTest (car (vlax-invoke obj 'Offset testDist)))
        
        (if offsetObjTest
          (progn
            (setq offsetDist testDist)
            (vla-delete offsetObjTest)
          )
          (progn
            (princ "\nОшибка: невозможно создать смещение.")
            (setq ent nil)
            (setq objectsToDelete nil)
            (restore-sysvars saved-vars)
            (continue)
          )
        )
      )
    )
    
    (setq offsetObj (car (vlax-invoke obj 'Offset offsetDist)))
    
    (if (null offsetObj)
      (progn
        (princ "\nОшибка при создании смещения.")
        (setq ent nil)
        (setq objectsToDelete nil)
        (restore-sysvars saved-vars)
        (continue)
      )
    )
    
    (setq offsetEname (vlax-vla-object->ename offsetObj))
    (setq objectsToDelete (cons offsetEname objectsToDelete))
    
    (setq vertices1 (get-polyline-vertices ent))
    (setq vertices2 (get-polyline-vertices offsetEname))
    
    (if (or (null vertices1) (null vertices2))
      (progn
        (princ "\nОшибка: не удалось получить вершины полилиний.")
        (setq ent nil)
        (setq objectsToDelete nil)
        (restore-sysvars saved-vars)
        (continue)
      )
    )
    
    (if (/= (length vertices1) (length vertices2))
      (progn
        (if (> (length vertices1) (length vertices2))
          (setq vertices2 (adjust-vertices-count vertices2 (length vertices1)))
          (setq vertices1 (adjust-vertices-count vertices1 (length vertices2)))
        )
      )
    )
    
    (setq newVertices vertices1)
    
    (setq i (length vertices2))
    (while (> i 0)
      (setq i (1- i))
      (setq newVertices (append newVertices (list (nth i vertices2))))
    )
    
    (setq newVertices (append newVertices (list (car vertices1))))
    
    (setq closedPoly (create-closed-polyline newVertices sourceLayer))
    
    (if (null closedPoly)
      (progn
        (princ "\nОшибка: не удалось создать замкнутую полилинию.")
        (setq ent nil)
        (setq objectsToDelete nil)
        (restore-sysvars saved-vars)
        (continue)
      )
    )
    
    (if objectsToDelete
      (foreach objToDel objectsToDelete
        (if (entget objToDel)
          (entdel objToDel)
        )
      )
    )
    
    (setq resultPoly closedPoly)
    
    (if resultPoly
      (progn
        (setq ent resultPoly)
        (sssetfirst nil (ssadd resultPoly))
      )
    )
    
    (if resultPoly
      (progn
        (setq emptySelection (ssadd))
        (sssetfirst nil emptySelection)
        (setq emptySelection nil)
      )
    )
    
    (princ "\n")
    (initget "Да Нет")
    (setq answer (getkword "Повторить операцию? [Да/Нет] <Да>: "))
    
    (if (or (null answer) (eq answer "Да"))
      (progn
        (setq ent nil)
        (setq objectsToDelete nil)
        (setq continue T)
      )
      (setq continue nil)
    )
    
    (restore-sysvars saved-vars)
  )
  
  (princ "\nОперация завершена.")
  (princ)
)

(defun get-polyline-vertices (ent / obj objType vertices coords i count)
  (setq obj (vlax-ename->vla-object ent))
  (setq objType (vla-get-ObjectName obj))
  (setq vertices '())
  
  (cond
    ((= objType "AcDbPolyline")
      (setq coords (vlax-get obj 'Coordinates))
      (setq i 0)
      (while (< i (length coords))
        (setq vertices (append vertices (list (list (nth i coords) (nth (1+ i) coords)))))
        (setq i (+ i 2))
      )
    )
    ((= objType "AcDbLine")
      (setq vertices (list 
        (vlax-get obj 'StartPoint)
        (vlax-get obj 'EndPoint)
      ))
    )
    ((= objType "AcDb2dPolyline")
      (setq count (vla-get-NumberOfVertices obj))
      (setq i 0)
      (while (< i count)
        (setq pt (vlax-get obj 'Coordinate i))
        (setq vertices (append vertices (list pt)))
        (setq i (1+ i))
      )
    )
    (t
      (setq param 0.0)
      (while (setq pt (vlax-curve-getPointAtParam ent param))
        (setq vertices (append vertices (list pt)))
        (setq param (1+ param))
      )
    )
  )
  
  vertices
)

(defun create-closed-polyline (vertices layer / vertexCount vertexList)
  (setq vertexCount (length vertices))
  
  (if (< vertexCount 2)
    (progn
      (princ "\nОшибка: недостаточно вершин для создания полилинии.")
      (return nil)
    )
  )
  
  (setq vertexList '())
  (foreach v vertices
    (setq vertexList (append vertexList (list (cons 10 v))))
  )
  
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 8 layer)
        (cons 90 vertexCount)
        (cons 70 1)
      )
      vertexList
    )
  )
)

(defun adjust-vertices-count (vertices target-count / current-count step new-vertices i)
  (setq current-count (length vertices))
  
  (if (<= current-count 1)
    (return vertices)
  )
  
  (setq new-vertices '())
  (setq step (/ (float current-count) target-count))
  (setq i 0)
  
  (while (< i target-count)
    (setq idx (fix (* i step)))
    (if (>= idx current-count)
      (setq idx (1- current-count))
    )
    (setq new-vertices (append new-vertices (list (nth idx vertices))))
    (setq i (1+ i))
  )
  
  new-vertices
)

;; Алиасы poff (русские клавиши: зщаа)
(defun c:зщаа () (c:poff))

;; =====================================================================
;; DOFF — офсет в обе стороны + замкнутый контур (труба)
;; =====================================================================
;;; После выполнения команды результат НЕ выделяется и НЕ зумируется.
;;; Просто создаётся замкнутая полилиния, исходный объект — на слое К_осевая.
(defun c:doff (/ *error* saved-vars ent dist obj offset1 offset2
                            offset1_ename offset2_ename
                            old-layer src-layer result-pl
                            verts1 verts2 verts2rev all-verts
                            ent-list v)

  (setq saved-vars (save-sysvars '(CMDECHO OSMODE ORTHOMODE COORDS SNAPMODE GRIDMODE PEDITACCEPT)))
  (setq old-layer (getvar "CLAYER"))

  (defun *error* (msg)
    (setvar "CLAYER" old-layer)
    (restore-sysvars saved-vars)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (setvar 'CMDECHO 0)
  (setvar 'OSMODE 0)
  (setvar 'PEDITACCEPT 1)

  ;; ---------- выбор объекта ----------
  (setq ent (car (entsel "\nВыберите полилинию или отрезок: ")))
  (if (not ent)
    (progn
      (princ "\nОбъект не выбран.")
      (restore-sysvars saved-vars)
      (setvar "CLAYER" old-layer)
      (exit)
    )
  )

  (setq obj (vlax-ename->vla-object ent))

  (if (not (member (vla-get-ObjectName obj) '("AcDbPolyline" "AcDbLine" "AcDb2dPolyline")))
    (progn
      (princ "\nВыбран не полилиния и не отрезок.")
      (restore-sysvars saved-vars)
      (setvar "CLAYER" old-layer)
      (exit)
    )
  )

  (setq src-layer (cdr (assoc 8 (entget ent))))

  ;; ---------- расстояние смещения ----------
  (setq dist (getdist "\nВведите расстояние смещения: "))
  (if (not dist)
    (progn
      (princ "\nРасстояние не задано.")
      (restore-sysvars saved-vars)
      (setvar "CLAYER" old-layer)
      (exit)
    )
  )

  ;; ---------- смещение в обе стороны ----------
  (setq offset1 (car (vlax-invoke obj 'Offset dist)))
  (setq offset2 (car (vlax-invoke obj 'Offset (- dist))))

  (if (or (null offset1) (null offset2))
    (progn
      (princ "\nОшибка при создании смещений.")
      (restore-sysvars saved-vars)
      (setvar "CLAYER" old-layer)
      (exit)
    )
  )

  (vla-put-layer offset1 src-layer)
  (vla-put-layer offset2 src-layer)

  (setq offset1_ename (vlax-vla-object->ename offset1))
  (setq offset2_ename (vlax-vla-object->ename offset2))

  ;; ---------- собираем вершины обоих смещений ----------
  (setq verts1 (jc-get-vertices offset1_ename))
  (setq verts2 (jc-get-vertices offset2_ename))
  (setq verts2rev (reverse verts2))

  (setq all-verts (append verts1 verts2rev))

  ;; ---------- строим замкнутую LWPOLYLINE ----------
  (setq ent-list
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 src-layer)
      (cons 90 (length all-verts))
      '(70 . 1)
    )
  )

  (foreach v all-verts
    (setq ent-list (append ent-list (list (cons 10 v))))
  )

  (setq result-pl (entmakex ent-list))

  (if (not result-pl)
    (progn
      (princ "\nОшибка: не удалось создать замкнутую полилинию.")
      (if offset1_ename (entdel offset1_ename))
      (if offset2_ename (entdel offset2_ename))
      (restore-sysvars saved-vars)
      (setvar "CLAYER" old-layer)
      (exit)
    )
  )

  ;; ---------- удаляем промежуточные объекты ----------
  (if offset1_ename (entdel offset1_ename))
  (if offset2_ename (entdel offset2_ename))

  ;; ---------- исходный объект — на слой К_осевая ----------
  (if (not (tblsearch "LAYER" "К_осевая"))
    (vl-cmdf "._-LAYER" "_N" "К_осевая" "_C" "1" "К_осевая" "")
  )
  (vl-cmdf "._CHPROP" ent "" "_LA" "К_осевая" "")

  (setvar "CLAYER" old-layer)

  ;; ---------- БЕЗ ЗУМА, БЕЗ ВЫДЕЛЕНИЯ ----------
  ;; результат не выделяется и не зумируется — как просили

  (restore-sysvars saved-vars)

  (princ "\nОперация завершена успешно!")
  (princ)
)

(defun jc-get-vertices (ent / obj objType res coords i n pt param)
  (setq obj (vlax-ename->vla-object ent))
  (setq objType (vla-get-ObjectName obj))
  (setq res nil)
  (cond
    ((= objType "AcDbPolyline")
     (setq coords (vlax-get obj 'Coordinates))
     (setq i 0)
     (while (< i (length coords))
       (setq res (append res (list (list (nth i coords)
                                        (nth (1+ i) coords)
                                        0.0))))
       (setq i (+ i 2))
     ))
    ((= objType "AcDbLine")
     (setq res (list (vlax-get obj 'StartPoint)
                     (vlax-get obj 'EndPoint))))
    ((= objType "AcDb2dPolyline")
     (setq n (vla-get-NumberOfVertices obj))
     (setq i 0)
     (while (< i n)
       (setq pt (vlax-get obj 'Coordinate i))
       (setq res (append res (list pt)))
       (setq i (1+ i))
     ))
    (t
     (setq param 0)
     (while (setq pt (vlax-curve-getPointAtParam ent param))
       (setq res (append res (list pt)))
       (setq param (1+ param))
     ))
  )
  res
)

;; Алиасы doff (русские клавиши: вщаа)
(defun c:вщаа () (c:doff))

;; =====================================================================
;; ПРИ ЗАГРУЗКЕ
;; =====================================================================
(princ "\n========================================================")
(princ "\n  POFF / DOFF загружены")
(princ "\n========================================================")
(princ "\n  poff / зщаа  — офсет в одну сторону с объединением в полилинию")
(princ "\n  doff / вщаа  — офсет в обе стороны с объединением в полилинию")
(princ "\n========================================================")
(princ)
