import type { Heteronym } from "common";
import objectHash from "object-hash";
import { DB } from "../db";
import { useEffect, useLayoutEffect, useState } from "react";
import { View, FlatList } from "react-native";
import { List, Text } from "react-native-paper";

export default function Home({ navigation }) {
  const [input, setInput] = useState("");
  const [heteronyms, setHeteronyms] = useState([] as Heteronym[]);
  const [loading, setLoading] = useState(false);
  function getHets(tokens: string[]) {
    // I don't know why, but when I add the JOIN in the SQL in
    // getHeteronyms it just hangs.
    return DB.crossDbAll(
      `
select title, "from", null as lang, props
from heteronyms
where "from" is not null
and title LIKE ?
`,
      tokens.map((x) => `${x}%`)
    ) as Promise<Heteronym[]>;
  }
  useEffect(() => {
    if (input === "") return;
    setLoading(true);
    getHets([input]).then((res) => {
      setHeteronyms(res);
      setLoading(false);
    });
  }, [input]);
  useLayoutEffect(() => {
    navigation.setOptions({
      headerSearchBarOptions: {
        autoCapitalize: "none",
        autoFocus: true,
        disableBackButtonOverride: true,
        placeholder: "Search",
        onChangeText: (event) => setInput(event.nativeEvent.text),
      },
    });
  }, [navigation]);
  return (
    <>
      <View
        style={{
          marginLeft: 10,
          marginRight: 10,
          flex: 1,
          flexDirection: "row",
          alignItems: "center",
        }}
      >
        {"" === input ? (
          <View style={{ flex: 1, alignItems: "center", gap: 12 }}>
            <View style={{ alignItems: "center" }}>
              <Text variant="displayMedium">Kemdict</Text>
              <Text variant="headlineSmall">國語整合典</Text>
              {loading && <Text variant="headlineSmall">Loading</Text>}
            </View>
          </View>
        ) : (
          <FlatList
            data={heteronyms}
            keyExtractor={(x) =>
              objectHash(x, {
                // This is just to get an identifier for the data that
                // we control. MD5 is fast and adequate for this.
                algorithm: "md5",
                unorderedObjects: false,
              })
            }
            renderItem={({ item }) => (
              <View>
                <List.Item
                  title={item.title}
                  description={JSON.stringify(item)}
                />
              </View>
            )}
          />
        )}
      </View>
    </>
  );
}
