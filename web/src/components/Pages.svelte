<script>
  import { range } from "lodash-es";
  import clsx from "clsx";
  export let activePage = 1;
  export let pageCount = 3;
  export let maxDisplayedCount = 5;
  export let url;
  export let baseURL;

  /// Return the list of pages that should be displayed given `page`,
  /// the current page.
  function currentDisplayedPages(page) {
    return range(1, pageCount + 1);
  }

  function linkToPage(page) {
    let localUrl = new URL(url);
    if (import.meta.env.PROD) {
      localUrl = new URL(url.pathname + url.search, baseURL);
    }
    localUrl.searchParams.set("p", page);
    return localUrl.toString();
  }

  const chevron_back_outline = `<svg class="h-4 w-4" viewBox="0 0 512 512"
      ><path
        fill="none"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-linejoin="round"
        stroke-width="48"
        d="M328 112L184 256l144 144"
      /></svg>
  `;
  const chevron_forward_outline = `<svg class="h-4 w-4" viewBox="0 0 512 512"
      ><path
        fill="none"
        stroke="currentColor"
        stroke-linecap="round"
        stroke-linejoin="round"
        stroke-width="48"
        d="M184 112l144 144-144 144"
      /></svg
    >
  `;

  const a = clsx(
    "inline-flex items-center justify-center",
    "rounded-full",
    "min-h-[2em] min-w-[2em]"
  );
  const disabled = "opacity-50";
  const hover = "hover:bg-gray-100 hover:dark:bg-stone-800";
</script>

<div class="flex flex-wrap items-center justify-center space-x-2">
  {#if activePage === 1}
    <span class={clsx(a, disabled)}>
      {@html chevron_back_outline}
    </span>
  {:else}
    <a class={clsx(a, hover)} href={linkToPage(activePage - 1)}>
      {@html chevron_back_outline}
    </a>
  {/if}
  {#each currentDisplayedPages(activePage) as page}
    <a
      class={clsx(
        a,
        hover,
        page === activePage && [
          "bg-surface-200 dark:bg-surface-900",
          "hover:bg-surface-300 hover:dark:bg-surface-800",
        ]
      )}
      href={linkToPage(page)}>{page}</a
    >
  {/each}
  {#if activePage === pageCount}
    <span class={clsx(a, disabled)}>
      {@html chevron_forward_outline}
    </span>
  {:else}
    <a class={clsx(a, hover)} href={linkToPage(activePage + 1)}>
      {@html chevron_forward_outline}
    </a>
  {/if}
</div>
