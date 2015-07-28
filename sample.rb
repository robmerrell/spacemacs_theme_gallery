# Sample highlightable code with keywords,
# operators, strings, numbers, etc.
module MyMath
  def self.sum_array(arr)
    arr.reduce(0) { |acc, x| x + acc }
  end
end

puts "Sum is: #{MyMath.sum_array([1, 2, 3])}"
