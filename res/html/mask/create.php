<form action="<?php echo webroot("private/create.php")?>"
		method="post" enctype="multipart/form-data">
	<p>
        	<input type="text" placeholder="URL"
			name="url_target" class="basic-text">
	</p>

	<p>
		<input type="text" placeholder="Alias (Shortened URL)"
			name="url_alias" class="basic-text">
	</p>

        <input style="margin-top: 10px; margin-bottom: 10px" type="submit"
         value="FORGE <?php echo(strtoupper($GLOBALS["url_aliasize_item"])); ?>" name="submit">
</form>
