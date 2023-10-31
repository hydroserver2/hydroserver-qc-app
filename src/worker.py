print("Worker thread...")
import js
# import sys
from pyscript import display, sync, window, document, when

# display(sys.version)

@when("click", "#my_button")
def click_handler(event):
    # for attr in dir(js):
    #   print("obj.%s = %r" % (attr, getattr(js, attr)))
    sync.alert_message("Hello from Python!")

my_element = document.querySelector("#my-id")
my_element.innerText = "Modifying DOM from Python. Here is your host name: " + window.location.hostname

# Import and use JS function in Python
# from js import name, addTwoNumbers, console
# console.log("Hello " + name + ".Adding 1 and 2 in Javascript: " + str(addTwoNumbers(1, 2)))

# def multiplyTwoNumbers(x, y):
#   return (x * y)