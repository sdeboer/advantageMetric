page "/templates/*", layout: false

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

activate :livereload
config[:debug_assets] = true
config[:file_watcher_ignore] += [/\.swp$/, /\.un~$/, /\.git\//]

set :css_dir, 'stylesheets'

set :js_dir, 'javascripts'

set :images_dir, 'images'

# Build-specific configuration
configure :build do
  #activate :minify_css

  #activate :minify_javascript

  #activate :asset_hash

  activate :relative_assets
	activate :gzip

  # Or use a different image path
  # set :http_path, "/Content/images/"
end

activate :s3_sync do |s3_sync|
  s3_sync.bucket                     = 'advantage.psiinteractive.com' # The name of the S3 bucket you are targetting. This is globally unique.
  s3_sync.region                     = 'us-east-1'     # The AWS region for your bucket.
end
