<script>
  function setDarkMode() {
    // This is copied into BaseLayout as well to avoid FOUC.
    if (
      localStorage.theme === "dark" ||
      ((!localStorage.theme || localStorage.theme === "auto") &&
        window.matchMedia("(prefers-color-scheme: dark)").matches)
    ) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }
  const states = Object.freeze({
    auto: "auto",
    light: "light",
    dark: "dark",
    isAuto: (obj) => !obj || obj === "auto",
  });
  let theme = states.auto;
  let system = states.auto;
  if (typeof window !== "undefined") {
    if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
      system = states.dark;
    } else {
      system = states.light;
    }
  }
  if (typeof localStorage !== "undefined") {
    theme = localStorage.getItem("theme");
  }
  function rotateDarkMode() {
    if (system === states.dark) {
      if (states.isAuto(theme)) {
        theme = states.light;
      } else {
        theme = states.auto;
      }
    } else {
      if (states.isAuto(theme)) {
        theme = states.dark;
      } else {
        theme = states.auto;
      }
    }
    localStorage.setItem("theme", theme);
    setDarkMode();
  }
  $: isDark = theme === "dark" || (states.isAuto(theme) && system === "dark");
  $: loading = states.isAuto(system);
</script>

<button
  class="btn bg-yellow-300 p-1 hover:bg-yellow-400 dark:bg-orange-900 dark:hover:bg-orange-800"
  type="button"
  role="switch"
  title="切換暗色模式"
  aria-label="切換暗色模式"
  aria-checked={isDark}
  on:click={rotateDarkMode}
>
  {#if loading}
    <div class="h-5 w-5"></div>
  {:else if isDark}
    <!-- ionicons moon-outline and sunny-outline. -->
    <svg
      class="h-5 w-5"
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 512 512"
      ><path
        d="M160 136c0-30.62 4.51-61.61 16-88C99.57 81.27 48 159.32 48 248c0 119.29 96.71 216 216 216 88.68 0 166.73-51.57 200-128-26.39 11.49-57.38 16-88 16-119.29 0-216-96.71-216-216z"
        fill="#da6800"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-linejoin="round"
        stroke-width="32"
      />
    </svg>
  {:else}
    <svg
      class="h-5 w-5"
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 512 512"
      ><path
        fill="none"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-miterlimit="10"
        stroke-width="32"
        d="M256 48v48M256 416v48M403.08 108.92l-33.94 33.94M142.86 369.14l-33.94 33.94M464 256h-48M96 256H48M403.08 403.08l-33.94-33.94M142.86 142.86l-33.94-33.94"
      /><circle
        cx="256"
        cy="256"
        r="80"
        fill="yellow"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-miterlimit="10"
        stroke-width="32"
      /></svg
    >
  {/if}
</button>
