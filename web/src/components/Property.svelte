<script lang="ts">
  import clsx from "clsx";
  const props: {
    key: string;
    value?: string | undefined;
    class?: string;

    /**
     * Only render if this value is truthy.
     * This is as if this is wrapped in an #if block with this value.
     * Defaults to true, so that it doesn't interfere with other criteria.
     */
    if?: unknown | undefined;

    valueClass?: string;
    html?: boolean;
    children?: any;
  } = $props();
  const {
    key,value,class: klass, valueClass, html, children
  } = props;

  // We have to do this instead of destructuring to distinguish between
  // passing undefined and not passing this.
  const ifValue = "if" in props ? props.if : true
</script>

{#if ifValue}
  {#if value || children}
    <div class={clsx("mb-4 flex items-baseline", klass)}>
      <span class="colorPropertyKey mr-2 px-2 py-1">{key}</span>
      <span class={clsx("prose", valueClass)}>
        {#if children}
          {@render children()}
        {:else}
          {#if html}{@html value}{:else}{value}{/if}
        {/if}</span
      >
    </div>
  {/if}
{/if}
