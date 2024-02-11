;;; find-entry.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zach Dingels

;; Author: Zach Dingels
;; Keywords: org
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Navigate org documents with minimal keystrokes like you would navigate a fileystem with IDO.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of Contents
;; -----------------------------------------------------------------------------
;; 1. constants
;; 2. parser
;; 3. entry-node
;; 4. entry-graph
;; 5. entry-tree-query
;; 6. entry-path
;; 7. org navigation
;; 8. find-entry
;; -----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst fe--org-heading-char 42
  "The ascii value of *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fe--point-at-org-heading-char ()
  (= (char-after) fe--org-heading-char))

(defun org-entry-level-at-point ()
  "The number of * for the org entry on the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((pos-init (point)))
      (while (fe--point-at-org-heading-char) (forward-char))
      (- (point) pos-init))))

(defun org-entry-title-at-point ()
  "The title of the org entry on the current line."
  (org-entry-get nil "ITEM"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry-node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defstruct entry-node
  "A entry in an org document represented as a node in a graph."
  id
  line
  title
  level
  parent-id)

(defvar entry-node-root
  (make-entry-node :id 0 :line -1 :title "root" :level 0 :parent-id 0)
  "A dummy root node")

(defvar fe--id-counter 0
  "Used to track how many headings have been viewed.")

(defun make-entry-node-at-point ()
  (make-entry-node :id (cl-incf fe--id-counter)
                   :line (line-number-at-pos)
                   :title (org-entry-title-at-point)
                   :level (org-entry-level-at-point)
                   :parent-id nil))

(defun parse-entry-nodes-from-buffer ()
  "Returns a list of unconnected entry-nodes generated from the current buffer."
  (org-map-entries #'make-entry-node-at-point t 'file))

(defun entry-node-set-parent (node parent)
  "Returns a copy of node with parend-id set to parent's id."
  (if parent (make-entry-node :id	 (entry-node-id    node)
                              :line      (entry-node-line  node)
                              :title	 (entry-node-title node)
                              :level	 (entry-node-level node)
                              :parent-id (entry-node-id    parent))
    node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry-graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connect-entry-nodes (nodes)
  "Connects a list of in order nodes to their parents."
  (cl-loop
   with stack = (list entry-node-root)
   with node-pairs = (-zip-fill entry-node-root nodes (cdr nodes))

   for pair in node-pairs
   for node = (car pair)
   for node-next = (cdr pair)
   for node-parent = (first stack)

   collect (entry-node-set-parent node node-parent)

   ;; Update the stack
   for level-diff = (- (entry-node-level node)
                       (entry-node-level node-next))
   for op = (if (< 0 level-diff)
                (lambda () (pop stack))
              (lambda () (push node stack)))
   do (dotimes (i (abs level-diff))
        (funcall op))))

(defun make-entry-tree-from-buffer ()
  "Parses the current buffer and returns a tree of entry-nodes"
  (setq fe--id-counter 0)
  (connect-entry-nodes (parse-entry-nodes-from-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry-tree-query
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun identity (x) x)
(defun entry-tree-query (map id title level parent-id)
  "Queries an entry map by matching the specified arguments"
  (let ((id-filter		(if id		(lambda (x) (eq (entry-node-id x) id))			#'identity))
        (title-filter		(if title	(lambda (x) (equal (entry-node-title x) title))		#'identity))
        (level-filter		(if level	(lambda (x) (eq (entry-node-level x) level))		#'identity))
        (parent-id-filter	(if parent-id   (lambda (x) (eq (entry-node-parent-id x) parent-id))    #'identity)))
    (->> map
         (-filter id-filter)
         (-filter title-filter)
         (-filter level-filter)
         (-filter parent-id-filter))))

(defun entry-tree-get-children (map node)
  "Returns a list of children of the given node in the given map."
  (let ((node-id (if node (entry-node-id node) nil)))
    (entry-tree-query map nil nil nil node-id)))

(defun entry-tree-get-parent (map node)
  "Returns the parent of the given node in the given graph."
  (when (entry-node-parent-id node)
    (car (entry-tree-query map (entry-node-parent-id node) nil nil nil))))

(defun entry-tree-find-id (map id)
  "Returns the node with the given id in the given map if it exists."
  (entry-tree-query map id nil nil nil))

(defun entry-tree-parentp (map node)
  (not (null (entry-tree-get-children map node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun entry-tree-get-path (map node)
  "Returns the path from the root to the given node in the given map."
  (cl-loop
   while node
   do (setq node (entry-tree-get-parent map node))
   collect node into nodes
   finally return (reverse (butlast nodes))))

(defun entry-path-to-string (ancestory)
  "Given a list of nodes create a string representation."
  (string-join (mapcar #'entry-node-title ancestory) "/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org navitaion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fe--goto-line (line)
  (interactive)
  ;; Fold all content, only show heading
  (org-global-cycle 0)
  ;; Go to the heading that covers that line
  (goto-line line)
  ;; Fold all headings
  (org-shifttab)
  ;; Open the current heading
  (outline-show-entry))

(defun fe--goto-node (node)
  (fe--goto-line (entry-node-line node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-entry ()
  (interactive)
  ;; Get the path from the user
  (setq map (make-entry-tree-from-buffer)
        node entry-node-root
        path '()
        running t)
  (while running
    (let* ((prompt  (concat "Find entry: " (entry-path-to-string (reverse path)) "/"))
           (children (cons node (entry-tree-get-children map node)))
           (options-map (mapcar (lambda (n) (cons (entry-node-title n) (entry-node-id n))) children))
           ;; Replace current node title with "."
           (options-map (cons (cons "." (cdar options-map)) (cdr options-map)))
           (options-text (mapcar #'car options-map))
           (choice  (ido-completing-read prompt options-text))
           (choice (car (entry-tree-find-id map (alist-get choice options-map)))))
      (setq running (and (entry-tree-parentp map choice) (not (equal node choice)))
            node choice
            path (cons node path))))
  (when (not (equal node entry-node-root))
    (fe--goto-node node)))

(provide 'find-entry)
;;; find-entry.el ends here
