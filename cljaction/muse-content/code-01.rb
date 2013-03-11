def with_connection (user_id)
  puts "with conn: creating connection for "  + user_id.to_s
  conn = "the connection for " + user_id.to_s
  yield conn
  puts "with conn: closing " + conn
end

user_id = 10
with_connection(user_id) do |conn|
  puts "caller: hiya"
  puts "caller: using " + conn
end
