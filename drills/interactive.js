$(document).ready(function() {
  
    // Hide hints & provide ability to show them
    $("li p.hint").wrapInner("<span></span>").
      prepend("<a class='toggle-hint' href='#'>Show hint</a>.  ");

    $("li p.hint span").css("display", "none");

    $("a.toggle-hint").click(function() {
      hint = $(this).closest("p").find("span");
      hint.css("display", "inline");
      return(false);
    });

    // Hide answers, and provide link to show them.
    $("pre.answer").css("display", "none")
    $("li p:first-child").
      append("<span>  <a class='toggle' href='#'>Show answer</a>.</span>");
    $("a.toggle").click(function() {
      code = $(this).closest("li").find("pre.answer");
      if (code.css("display") == "none") {
        code.slideDown();
        $(this).text("Hide answer");          
      } else {
        code.slideUp();
        $(this).text("Show answer");
      }
      return(false);
    })
});
