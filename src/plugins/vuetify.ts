import 'vuetify/styles'
import '@mdi/font/css/materialdesignicons.css'
import { aliases, mdi } from 'vuetify/iconsets/mdi'

import { createVuetify, ThemeDefinition } from 'vuetify'
import * as directives from 'vuetify/directives'
import * as components from 'vuetify/components'

const light: ThemeDefinition = {
  dark: false,
  colors: {
    primary: '#3fd',
    accent: '#3fd',
    secondary: '#3fd',
    info: '#3fd',
    warning: '#3fd',
    error: '#3fd',
    success: '#3fd',
  },
}

const dark: ThemeDefinition = {
  dark: true,
  colors: {
    primary: '#33c',
    accent: '#33c',
    secondary: '#33c',
    info: '#33c',
    warning: '#33c',
    error: '#33c',
    success: '#33c',
  },
}

export default createVuetify({
  components,
  directives,
  aliases: {},
  icons: {
    defaultSet: 'mdi',
    aliases,
    sets: {
      mdi,
    },
  },
  defaults: {},
  theme: {
    defaultTheme: 'light',
    themes: {
      light,
      dark,
    },
    variations: {
      colors: ['primary', 'secondary', 'surface'],
      lighten: 6,
      darken: 6,
    },
  },
})
