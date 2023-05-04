import { List, Appbar } from "react-native-paper";
import useSWR from "swr";
import { FlatList, Text, View } from "react-native";
import { crossDbAll } from "../db.ts";

export default function Home() {
  const { data, isLoading } = useSWR("dummy", async () => {
    return await crossDbAll(`select * from "heteronyms" limit 10;`);
  });
  return (
    <>
      {isLoading ? (
        <View>
          <Text>Loading</Text>
        </View>
      ) : (
        <FlatList
          ListHeaderComponent={
            <Appbar.Header>
              <Appbar.Content title="Kemdict" />
            </Appbar.Header>
          }
          data={data}
          renderItem={({ item }) => (
            <View>
              <List.Item
                title={item.title}
                description={JSON.stringify(item)}
              />
            </View>
          )}
          keyExtractor={(item) => item.title}
        ></FlatList>
      )}
    </>
  );
}
