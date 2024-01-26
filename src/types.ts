export type _Window = Window &
  typeof globalThis & { pyscript: any; loadPyodide: any; pyGlobals: any } & {
    [key: string]: any
  }
