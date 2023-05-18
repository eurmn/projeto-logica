import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';
import unoCSS from 'unocss/vite';

export default defineConfig({
  plugins: [
    sveltekit(),
    unoCSS()
  ]
});
