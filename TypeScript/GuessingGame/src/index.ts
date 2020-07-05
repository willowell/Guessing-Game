import _ from "lodash";

import fetch from "node-fetch";
import osLocale from 'os-locale';
import publicIp from "public-ip";


const getData = async (url: string): Promise<any> => {
  try {
    const response = await fetch(url);
    const json = await response.json();
    return json;
  } catch (error) {
    console.log(error);
    return null;
  }
};


async function main() {
    const getIPURL = (ip: string, fields: string) => (
        `http://ip-api.com/json/${ip}?fields=${fields}`
    );

    const fields = "status,message,continent,country,countryCode,region,regionName,city,district,zip,lat,lon,timezone,isp,org,as,asname,mobile,query"

    console.log(
        _.reduce(_.range(1, 11), (acc, n) => acc + n, 0)
    );

    const ip = await publicIp.v4();
    const loc = await osLocale();

    const geoData = await getData(getIPURL(ip, fields));

    const lang = await getData(`https://fourtonfish.com/hellosalut/?lang=${loc.slice(0, 2)}`);

    console.log(geoData);

    if (geoData.country === "Canada") {
        console.log(`${lang.hello}, person from Canada living in ${geoData.city}, ${geoData.regionName}!`);
    }
}

main();

