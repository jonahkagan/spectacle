open OUnit
open Transforms

let assert_equal_spec = assert_equal ~printer:Spec.pretty

let bb_tests =
"Backbone" >::: [
  "key empty" >:: (fun () ->
    assert_equal
      (transform_bb
        None
        (SObject []))
      (TArrow
        ([TObject []],
        (TObject [
          ("attributes", Present, TObject []);
          ("cid", Present, tStr);
          ("id", Absent, TTop)
        ])))
  );
  "key done,title" >:: (fun () ->
    assert_equal
      (transform_bb
        None
        (SObject [
          ("done", SBoolean);
          ("title", sStr)
        ]))
      (TArrow
        ([TObject [
          ("done", Present, TBoolean);
          ("title", Present, tStr)
        ]],
        (TObject [
          ("attributes", Present, TObject [
            ("done", Present, TBoolean);
            ("title", Present, tStr)
          ]);
          ("cid", Present, tStr);
          ("id", Absent, TTop)
        ])))
  );
  "key _id" >:: (fun () ->
    assert_equal
      (transform_bb
        None
        (SObject [("_id", SNumber)]))
      (TArrow
        ([TObject [("_id", Present, TNumber)]],
        (TObject [
          ("attributes", Present, TObject [("_id", Present, TNumber)]);
          ("cid", Present, tStr);
          ("id", Absent, TTop)
        ])))
  );
  "bb value idAttr present" >:: (fun () ->
    assert_equal
      (transform_bb
        (Some (SObject [("idAttribute", sSingle "_id")]))
        (SObject [("_id", SNumber)]))
      (TArrow
        ([TObject [("_id", Present, TNumber)]],
        (TObject [
          ("attributes", Present, TObject [("_id", Present, TNumber)]);
          ("cid", Present, tStr);
          ("id", Present, TNumber)
        ])))
  );
  "bb value idAttr absent" >:: (fun () ->
    assert_equal
      (transform_bb
        (Some (SObject [("idAttribute", sSingle "_id")]))
        (SObject []))
      (TArrow
        ([TObject [("_id", Absent, TTop)]],
        (TObject [
          ("attributes", Present, TObject [
            ("_id", Absent, TTop)
          ]);
          ("cid", Present, tStr);
          ("id", Absent, TTop)
        ])))
  );
  "bb value idAttr mismatch" >:: (fun () ->
    assert_equal
      (transform_bb
        (Some (SObject [("idAttribute", sSingle "_id")]))
        (SObject [("not_id", SNumber)]))
      (TArrow
        ([TObject [
          ("_id", Absent, TTop);
          ("not_id", Present, TNumber)
        ]],
        (TObject [
          ("attributes", Present, TObject [
            ("_id", Absent, TTop);
            ("not_id", Present, TNumber)
          ]);
          ("cid", Present, tStr);
          ("id", Absent, TTop)
        ])))
  );

]

let run_tests () = run_test_tt_main ("All" >::: [bb_tests])
