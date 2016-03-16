#!/usr/bin/env ruby
# e is a shorthand wrapper for emacsclient, which also handles (sloppily) copy-pasted file[:line[:colum]] arguments.
# By default, emacsclient --no-wait is passed. If this is not desired, -w|--wait needs to be passed as an argument.

emacsclient_args = "-n "
args = []

ARGV.each do |arg|
  if arg == "--wait" or arg == "-w"
    emacsclient_args = ""
  else
    args << arg
  end
end

command = "emacsclient #{emacsclient_args}"
args.each do |arg|
  if File.exists? arg
    command << " #{arg}"
    next
  end

  if arg =~ /:\d+:\d+:?/
    tmp = arg.split(':')
    filename = tmp[0...-2].join(':')
    line     = tmp[-2]
    column   = tmp[-1]
    command << " +#{line}:#{column} #{filename}"
  elsif arg =~ /:\d+:?$/
    tmp = arg.split(':')
    filename = tmp[0...-1].join(':')
    line     = tmp[-1]
    command << " +#{line} #{filename}"
  elsif arg =~ /:$/
    command << arg[0...-1]
  else
    command << " #{arg}"
  end
end

puts command
system(command)
