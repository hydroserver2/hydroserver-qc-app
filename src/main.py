print("Main thread...")
# import js
# from pyscript import display, sync, window, document, PyWorker

# def hello(name="world"):
#     display(f"Hello, {name}")

# worker = PyWorker("src/worker.py")
# worker.sync.hello = hello

# Import and use JS function in Python
from js import name, addTwoNumbers, console
console.log("Hello " + name + ".Adding 1 and 2 in Javascript: " + str(addTwoNumbers(1, 2)))

def multiplyTwoNumbers(x, y):
  return (x * y)

# js.mult = multiplyTwoNumbers