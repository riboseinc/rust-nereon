$(function() {
    var timer;
    var noc;
    $("#noc-in").keyup("change", function() {
        clearTimeout(timer);
        timer = setTimeout(function() {
            var new_noc = $("#noc-in").val();
            if (new_noc != noc) {
                noc = new_noc;
                $.post("parse", noc, function(result) {
                    $("#noc-out").text(result);
                });
            }
        }, 1000);
    });
});
