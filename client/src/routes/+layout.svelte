<script>
    import 'virtual:uno.css';
    import './app.css';
    import '@fontsource/inter/variable.css';
    import '@fontsource/familjen-grotesk/variable.css';
    import 'nprogress/nprogress.css';

    import NProgress from 'nprogress';
    import { navigating } from '$app/stores';

    NProgress.configure({
      minimum: 0.1,
      speed: 400,
      easing: 'ease-in-out',
    });

    $: {
      if ($navigating) {
        NProgress.start();
      } else NProgress.done();
    }
</script>

<slot />
