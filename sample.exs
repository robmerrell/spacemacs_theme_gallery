# This is a comment
defmodule Math do
  def sum(a, b), do: a + b

  @doc """
  Sum a list of numbers
  """
  def sum_list(list) do
    Enum.reduce list, &Kernel.+/2
  end
end
