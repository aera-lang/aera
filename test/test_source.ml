open Frontend
open Source
open Span
open Position

(* Helper Functions *)

let span start_ end_ = { start_; end_ }
let pos line col = { line; col }

let pp_span fmt (s : Span.t) =
    Format.fprintf fmt "{ start_ = %d; end_ = %d }" s.start_ s.end_

let equal_span (a : Span.t) (b : Span.t) =
    a.start_ = b.start_ && a.end_ = b.end_

let span_testable = Alcotest.testable pp_span equal_span

let pp_pos fmt (p : Position.t) =
    Format.fprintf fmt "{ line = %d; col = %d }" p.line p.col

let equal_pos (a : Position.t) (b : Position.t) =
    a.line = b.line && a.col = b.col

let pos_testable = Alcotest.testable pp_pos equal_pos

(* Testing Each Function *)

let check_line_spans msg expected src =
    Alcotest.(check (array span_testable)) msg expected src.line_spans

let check_line_from_offset msg expected offset src =
    Alcotest.(check (result int string)) msg expected (src |> line_from_offset offset)

let check_offset_to_pos msg expected offset src =
    Alcotest.(check (result pos_testable string)) msg expected (src |> offset_to_pos offset)

let check_span_to_pos msg expected sp src =
    Alcotest.(check (result (pair pos_testable pos_testable) string)) msg expected (src |> span_to_pos sp)

let check_extract msg expected sp src =
    Alcotest.(check string) msg expected (extract sp src)

let check_get_line msg expected line src =
    Alcotest.(check (result string string)) msg expected (src |> get_line line)

let check_line_containing msg expected offset src =
    Alcotest.(check (result span_testable string)) msg expected (src |> line_containing offset)

(* Line Spans Tests (via Create) *)

let test_single_line_no_newline () =
    let src = create "hello world" "test.aera" in
    check_line_spans "single line" [| span 0 11 |] src

let test_multi_line_no_trailing_newline () =
    let src = create "line1\nline2\nline3" "test.aera" in
    check_line_spans "multi line, no trailing newline"
        [| span 0 5; span 6 11; span 12 17 |] src

let test_multi_line_trailing_newline () =
    let src = create "line1\nline2\n" "test.aera" in
    check_line_spans "multi line, trailing newline"
        [| span 0 5; span 6 11; span 12 12 |] src

let test_empty_contents () =
    let src = create "" "test.aera" in
    check_line_spans "empty contents" [| span 0 0 |] src

let test_only_newlines () =
    let src = create "\n\n\n" "test.aera" in
    check_line_spans "only newlines"
        [| span 0 0; span 1 1; span 2 2; span 3 3 |] src

let test_blank_line_in_middle () =
    let src = create "abc\n\ndef" "test.aera" in
    check_line_spans "blank line in middle"
        [| span 0 3; span 4 4; span 5 8 |] src

let test_filename_and_contents_preserved () =
    let src = create "abc" "my_file.aera" in
    Alcotest.(check string) "contents" "abc" src.contents;
    Alcotest.(check string) "filename" "my_file.aera" src.filename

(* Line_From_Offset Sets *)

let src2 () = create "line1\nline2\nline3" "test.aera"

let test_line_from_offset_start () =
    check_line_from_offset "offset 0 -> line 1" (Ok 1) 0 (src2 ())

let test_line_from_offset_within_line () =
    check_line_from_offset "offset 4 -> line 1" (Ok 1) 4 (src2 ())

let test_line_from_offset_on_newline_char () =
    check_line_from_offset "offset on \\n -> still line 1" (Ok 1) 5 (src2 ())

let test_line_from_offset_start_of_next_line () =
    check_line_from_offset "offset 6 -> line 2" (Ok 2) 6 (src2 ())

let test_line_from_offset_last_line () =
    check_line_from_offset "offset 12 -> line 3" (Ok 3) 12 (src2 ())

let test_line_from_offset_end_of_source () =
    check_line_from_offset "offset == length -> line 3" (Ok 3) 17 (src2 ())

let test_line_from_offset_beyond_end () =
    check_line_from_offset "offset beyond end -> Error"
        (Error "offset is beyond the end of the source") 18 (src2 ())

let test_line_from_offset_negative () =
    check_line_from_offset "negative offset -> Error"
        (Error "offset is before the start of the source") (-1) (src2 ())

(* Offset_To_Pos Tests *)

let test_offset_to_pos_start () =
    check_offset_to_pos "offset 0" (Ok (pos 1 1)) 0 (src2 ())

let test_offset_to_pos_mid_line () =
    check_offset_to_pos "offset 4" (Ok (pos 1 5)) 4 (src2 ())

let test_offset_to_pos_second_line () =
    check_offset_to_pos "offset 6" (Ok (pos 2 1)) 6 (src2 ())

let test_offset_to_pos_last_line_end () =
    check_offset_to_pos "offset 17 (EOF)" (Ok (pos 3 6)) 17 (src2 ())

let test_offset_to_pos_out_of_range () =
    check_offset_to_pos "offset out of range"
        (Error "offset is beyond the end of the source") 100 (src2 ())

(* Span_To_Pos Tests *)

let test_span_to_pos_within_one_line () =
    check_span_to_pos "line2 span" (Ok (pos 2 1, pos 2 6)) (span 6 11) (src2 ())

let test_span_to_pos_across_lines () =
    check_span_to_pos "span crossing lines 1 -> 3"
        (Ok (pos 1 3, pos 3 3)) (span 2 14) (src2 ())

let test_span_to_pos_invalid_end () =
    check_span_to_pos "valid start, out-of-range end"
        (Error "offset is beyond the end of the source") (span 0 100) (src2 ())

(* Extract Tests *)

let test_extract_line () =
    check_extract "extract line2" "line2" (span 6 11) (src2 ())

let test_extract_partial () =
    check_extract "extract partial span" "ne1" (span 2 5) (src2 ())

let test_extract_empty_span () =
    check_extract "zero-width span" "" (span 4 4) (src2 ())

(* Get Line Tests *)
let test_get_line_first () =
    check_get_line "line 1" (Ok "line1") 1 (src2 ())

let test_get_line_last () =
    check_get_line "line 3" (Ok "line3") 3 (src2 ())

let test_get_line_blank () =
    let src = create "abc\n\ndef" "test.aera" in
    check_get_line "blank middle line" (Ok "") 2 src

let test_get_line_zero () =
    check_get_line "line 0 -> Error" (Error "line index out of range") 0 (src2 ())

let test_get_line_too_large () =
    check_get_line "line beyond count -> Error" (Error "line index out of range") 4 (src2 ())

(* Line Containing Tests *)

let test_line_containing_start_of_line () =
    check_line_containing "offset 6 -> line2 span" (Ok (span 6 11)) 6 (src2 ())

let test_line_containing_newline_char () =
    check_line_containing "offset on \\n -> still line2 span" (Ok (span 6 11)) 11 (src2 ())

let test_line_containing_out_of_range () =
    check_line_containing "offset out of range -> Error"
        (Error "offset is beyond the end of the source") 100 (src2 ())

let () =
    Alcotest.run "Source" [
        ("line_spans", [
            Alcotest.test_case "single line, no newline" `Quick test_single_line_no_newline;
            Alcotest.test_case "multi line, no trailing newline" `Quick test_multi_line_no_trailing_newline;
            Alcotest.test_case "multi line, trailing newline" `Quick test_multi_line_trailing_newline;
            Alcotest.test_case "empty contents" `Quick test_empty_contents;
            Alcotest.test_case "only newlines" `Quick test_only_newlines;
            Alcotest.test_case "blank line in middle" `Quick test_blank_line_in_middle;
            Alcotest.test_case "filename and contents preserved" `Quick test_filename_and_contents_preserved;
        ]);
        ("line_from_offset", [
            Alcotest.test_case "start of source" `Quick test_line_from_offset_start;
            Alcotest.test_case "within a line" `Quick test_line_from_offset_within_line;
            Alcotest.test_case "on newline char" `Quick test_line_from_offset_on_newline_char;
            Alcotest.test_case "start of next line" `Quick test_line_from_offset_start_of_next_line;
            Alcotest.test_case "last line" `Quick test_line_from_offset_last_line;
            Alcotest.test_case "end of source (EOF offset)" `Quick test_line_from_offset_end_of_source;
            Alcotest.test_case "beyond end" `Quick test_line_from_offset_beyond_end;
            Alcotest.test_case "negative offset" `Quick test_line_from_offset_negative;
        ]);
        ("offset_to_pos", [
            Alcotest.test_case "start" `Quick test_offset_to_pos_start;
            Alcotest.test_case "mid line" `Quick test_offset_to_pos_mid_line;
            Alcotest.test_case "second line" `Quick test_offset_to_pos_second_line;
            Alcotest.test_case "last line end (EOF)" `Quick test_offset_to_pos_last_line_end;
            Alcotest.test_case "out of range" `Quick test_offset_to_pos_out_of_range;
        ]);
        ("span_to_pos", [
            Alcotest.test_case "within one line" `Quick test_span_to_pos_within_one_line;
            Alcotest.test_case "across lines" `Quick test_span_to_pos_across_lines;
            Alcotest.test_case "invalid end offset" `Quick test_span_to_pos_invalid_end;
        ]);
        ("extract", [
            Alcotest.test_case "extract full line" `Quick test_extract_line;
            Alcotest.test_case "extract partial span" `Quick test_extract_partial;
            Alcotest.test_case "extract zero-width span" `Quick test_extract_empty_span;
        ]);
        ("get_line", [
            Alcotest.test_case "first line" `Quick test_get_line_first;
            Alcotest.test_case "last line" `Quick test_get_line_last;
            Alcotest.test_case "blank line" `Quick test_get_line_blank;
            Alcotest.test_case "line 0" `Quick test_get_line_zero;
            Alcotest.test_case "line beyond count" `Quick test_get_line_too_large;
        ]);
        ("line_containing", [
            Alcotest.test_case "start of line" `Quick test_line_containing_start_of_line;
            Alcotest.test_case "newline char" `Quick test_line_containing_newline_char;
            Alcotest.test_case "out of range" `Quick test_line_containing_out_of_range;
        ]);
    ]