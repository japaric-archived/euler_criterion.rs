
class Problem
  def code
    @result = yield
  end

  def expect(@expected)
  end

  getter expected
  getter result
end

def problem(number)
  #print "Problem #{number}: "
  print "Problem %04d: " % number
  problem = Problem.new

  begin
    problem.yield
    if problem.expected == problem.result
      puts "passed"
    else
      puts "failed (expected #{problem.expected} but got #{problem.result})"
    end
  rescue ex
    print "failed with exception (#{ex.message})"
  end
end
