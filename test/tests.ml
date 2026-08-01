let () =
    Alcotest.run "Aera" (Test_lexer.tests @ Test_source.tests)