
            $new(document).ready(function(){
                $new(".datepicker").datepicker({
            changeMonth: true,
            changeYear: true
        });
                $new("#company").change(function(){
                    var first = $new("#parent option:first").val();
                    $new("#parent").val(first);
                });
                $new("#parent").change(function(){
                    var first = $new("#company option:first").val();
                    $new("#company").val(first);
                });
           
$new(".machines input:checkbox").click(function(){
	var numChecked = 0;
	$new(".machines input:checkbox").each(function(){
		if($new(this).is(":checked")){
			numChecked++;
		}
	});
	if(numChecked > 0){
		$new("#infoStatus .chooseItem").hide();
	} else {
		$new("#infoStatus .chooseItem").show();
	}
});
 });