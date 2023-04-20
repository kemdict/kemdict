<script>
  import { range } from "lodash-es";
  import clsx from "clsx";
  import { format } from "$src/common";
  export let activePage = 1;
  export let pageCount = 3;
  export let pageLinkTemplate = "./$1";

  function linkToPage(page) {
    return format(pageLinkTemplate, `${page}`);
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

<div class="flex items-center justify-center space-x-2">
  {#if activePage === 1}
    <span class={clsx(a, disabled)}>
      {@html chevron_back_outline}
    </span>
  {:else}
    <a class={clsx(a, hover)} href={linkToPage(activePage - 1)}>
      {@html chevron_back_outline}
    </a>
  {/if}
  {#each range(1, pageCount + 1) as page}
    <a
      class={clsx(
        a,
        hover,
        page === activePage && [
          "bg-gray-200 dark:bg-stone-700",
          "hover:bg-gray-300 hover:dark:bg-stone-600",
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
