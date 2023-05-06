import type { Heteronym } from "common";
import { List, Appbar, Button } from "react-native-paper";
import useSWR from "swr";
import { FlatList, View } from "react-native";
import { Text, Searchbar } from "react-native-paper";

export default function Home({ navigation }) {
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
        <View style={{ flex: 1, alignItems: "center", gap: 12 }}>
          <View style={{ alignItems: "center" }}>
            <Text variant="displayMedium">Kemdict</Text>
            <Text variant="headlineSmall">國語整合典</Text>
          </View>
          <Searchbar
            value=""
            placeholder="搜尋…"
            onPressIn={() => navigation.navigate("Search")}
          />
        </View>
      </View>
    </>
  );
}
