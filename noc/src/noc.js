$(function() {
    var timer;
    $("#noc-in").keyup("change", function() {
        clearTimeout(timer);
        timer = setTimeout(function() {
            $.post("parse", $("#noc-in").val(), function(result) {
                $("#noc-out").text(result);
            });
        }, 1000);
    });
});
