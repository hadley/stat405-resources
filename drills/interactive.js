$(document).ready(function() {
    $("pre.answer").
      css("display", "none")
    $("li p").
      append("<span>  <a class='toggle' href='#'>Show answer</a>.</span>");
    $("a.toggle").click(function() {
      code = $(this).parent().parent().next();
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
