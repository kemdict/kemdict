<script lang="ts">
  import clsx from "clsx";
  let {
    key,
    value,
    class: klass,
    valueClass,
    html,
    children,
  }: {
    key: string;
    value?: string | undefined;
    class?: string;
    valueClass?: string;
    html?: boolean;
    children?: any;
  } = $props();
</script>

{#if value || children}
  <div class={clsx("mb-4 flex items-baseline", klass)}>
    <span class="mr-2 colorPropertyKey px-2 py-1">{key}</span>
    <span class={clsx("prose", valueClass)}>
      {#if children}
        {@render children()}
      {:else}
        {#if html}{@html value}{:else}{value}{/if}
      {/if}</span
    >
  </div>
{/if}
