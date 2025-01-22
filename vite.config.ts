/// <reference types="vitest" />

import { defineConfig } from 'vite'
import { resolve } from 'path'
import vue from '@vitejs/plugin-vue'
import vuetify from 'vite-plugin-vuetify'

export default defineConfig({
  plugins: [
    vue(),
    vuetify({
      autoImport: true,
      styles: { configFile: 'src/styles/settings.scss' },
    }),
  ],
  css: {
    preprocessorOptions: {
      scss: {
        api: 'modern-compiler', // or "modern"
      },
    },
  },
  optimizeDeps: {
    exclude: ['vuetify'],
  },
  server: {
    host: '127.0.0.1',
    port: 1203,
    /** @see https://docs.pyscript.net/2024.11.1/user-guide/workers/ */
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp',
      'Cross-Origin-Resource-Policy': 'cross-origin',
    },
  },
  resolve: {
    extensions: ['.js', '.json', '.vue', '.less', '.scss', '.ts'],
    alias: {
      '@': resolve(__dirname, 'src'),
    },
  },
  build: {
    manifest: true,
  },
  test: {
    globals: true,
    environmentMatchGlobs: [['src/components/**', 'jsdom']],
    server: {
      deps: {
        inline: ['vuetify'],
      },
    },
    environment: 'jsdom',
    coverage: {
      exclude: [
        '**/src/**/*.vue',
        '**/src/composables/useUserTags.ts',
        '**/src/plugins/**',
        '**/src/router/**',
        '**/src/store/**',
        '**/src/types/**',
        '**/src/config/**',
        '**/src/services/api.ts',
        '**/src/services/apiMethods.ts',
        '**/src/services/handle401.ts',
        '**/src/utils/mdi-icons.ts',
        '**/src/utils/materialColors.ts',
        '**/src/utils/CSVDownloadUtils.ts',
        '**/src/utils/plotting/graphSeriesUtils.ts',
        '**/src/utils/test/**',
        '**/src/utils/googleMaps/**',
        '**/src/utils/rules.ts',
        '**/src/App.vue',
        '**/src/main.ts',
        '**/src/vocabularies.ts',
        '**/*.d.ts',
        '**/postcss.config.js',
      ],
      thresholds: {
        lines: 80,
        statements: 80,
        functions: 80,
        branches: 80,
      },
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
    },
  },
})
