<script>
  export let heteronyms;
</script>

<!-- TODO: sementic variant and other variants -->
<div class="my-2">
  {#each heteronyms as het}
    <p><span>漢字：</span>{het.title}</p>
    <p><span>拼音：</span>{het.props.pinyin}</p>
    <p><span>部首：</span>{het.props.radical}</p>
    <p>
      <span>筆畫：</span>共{het.props.stroke_count}畫，部首外共{het.props
        .non_radical_stroke_count}畫
    </p>
    <p><span>倉頡：</span>{het.props.cangjie}</p>
    {#if het.props?.defs?.length == 1}
      <div><span>定義：</span>{het.props.defs[0]}</div>
    {:else if het.props?.defs?.length > 1}
      <div class="flex">
        <span>定義：</span>
        <ul>
          {#each het.props.defs as def}
            <li class="before:opacity-70 before:content-['-_']">{def}</li>
          {/each}
        </ul>
      </div>
    {/if}
  {/each}
</div>

<style lang="postcss">
  span {
    @apply font-bold text-gray-400;
  }
</style>
