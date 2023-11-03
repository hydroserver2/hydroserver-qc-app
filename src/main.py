print("Main thread...")

def handleDelete(data, index):
  if len(data) >= index + 1:
    del data[index]

def handleAddOne(data, index):
  if len(data) >= index + 1:
    data[index][1] = data[index][1] + 1