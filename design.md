# Design of blog site

The goal is to write a site for a blog that
is relatively easy to use.

The site will use acid-state for the db backend,
and snap for the web-server.

## Paths

posts/year/month/day/short_name

Internally, we will have an ordered map from
UTC-time to post ID, then a hash-map from post ID
to data about the post.

```` haskell
data Post =
 MkPost {
  post_time :: UTCTime
  post_zoned_time :: ZonedTime
  post_short_name :: Text
  post_title :: Text
  post_author :: UserID
  post_body :: BlobID
 }

data Posts =
 MkPosts {
  time_map :: Map.Map UTCTime PostID // enforce one post per time
  id_map :: H.HasMap PostID Post
 }
````

