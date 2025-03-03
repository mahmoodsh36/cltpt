(typep (cadr (cltpt::text-object-children (cltpt::parse "hello#(test) there 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end) what #(identity 'hello)"))) 'cltpt::text-block)

(cltpt::export-tree (cltpt::parse "hello#(test) there 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end) what #(identity 'hello)") 'latex)

(text-object-text (caddr (text-object-children (cltpt::parse "hello#(test) there 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end) what #(identity 'hello)"))))

;; get the text of the block in there
(text-object-text (cadr (text-object-sorted-children (cltpt::parse "hello#(test) there 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end-block) what #(identity 'hello)"))))

(export-tree (cltpt::parse "hello#(test) there 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end-block) what #(identity 'hello)") 'latex)