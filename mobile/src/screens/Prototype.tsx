import type { Heteronym } from "common";
import { List, Appbar } from "react-native-paper";
import useSWR from "swr";
import { FlatList, Text, View } from "react-native";
import { DB } from "../db";

export default function Home() {
  const { data, isLoading } = useSWR("dummy", async () => {
    return (await DB.crossDbAll(
      `select * from "heteronyms" limit 10;`
    )) as Heteronym[];
  });
  return (
    <>
      {isLoading ? (
        <View>
          <Text>Loading</Text>
        </View>
      ) : (
        <View>
          <FlatList
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
        </View>
      )}
    </>
  );
}
