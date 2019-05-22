<form action="<?php echo webroot("private/beam.php")?>"
		method="post" enctype="multipart/form-data">
	<p>
        	<input type="text" placeholder="Filename (Blank for uploaded name)" 
			name="desired_filename" class="basic-text">
	</p>

	<p>
		<input type="text" placeholder="Source (Optional)"
			name="file_source" class="basic-text">
	</p>

	<input type="file" name="uploadcoin" id="uploadcoin"
		style="margin: 0 auto">
	<input style="margin-top: 10px; margin-bottom: 10px" type="submit"
		value="INSERT <?php echo(strtoupper($GLOBALS["file_beam_item"])); ?>" name="submit">
</form>
