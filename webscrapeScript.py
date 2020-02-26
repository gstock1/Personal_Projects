from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup
from urllib.request import Request, urlopen


my_url = 'https://fortnitetracker.com/leaderboards'
req = Request(my_url, headers={'User-Agent': 'Mozilla/5.0'})
uClient = uReq(req)
page_html = uClient.read()
uClient.close()

page_soup = soup(page_html, "html.parser")

filename = "fortnitewinsleaderboard.csv"
f=open(filename, "w")
headers = "rank, player, country, wins, matches played\n"
f.write(headers)


containers =  page_soup.findAll("tr", {"class":"trn-table__row trn-lb-entry trn-lb-entry--top3"})

for container in containers:
	rank = container.td.text

	player = container.a.text

	try:
		image = container.findAll("td", {"class":"trn-lb-entry__player"})
		country = image[0].findAll("img", {"class":"trn-lb-entry__flag trn--mobile-hidden"})[0]["title"]
	except(IndexError):
		country = "___"

	matchInfo = container.findAll("td", {"class":"trn-lb-entry__stat trn-text--right"})
	wins = matchInfo[0].text
	matchesPlayed = matchInfo[1].text

	print("rank: " + rank + "   player: " + player + "   country: " + country + "   wins: " + wins + "   matches played: " + matchesPlayed)

	try :
		f.write( rank + "," + player + "," + country + "," + wins.replace("," ,"") + "," + matchesPlayed.replace("," ,"") + "\n")
	except(UnicodeEncodeError):
		f.write( rank + "," + "????" + "," + country + "," + wins.replace("," ,"") + "," + matchesPlayed.replace("," ,"") + "\n")


containers = page_soup.findAll("tr",{"class":"trn-table__row trn-lb-entry"})

for container in containers:
	rank = container.td.text

	player = container.a.text

	try:
		image = container.findAll("td", {"class":"trn-lb-entry__player"})
		country = image[0].findAll("img", {"class":"trn-lb-entry__flag trn--mobile-hidden"})[0]["title"]
	except(IndexError):
		country = "___"

	matchInfo = container.findAll("td", {"class":"trn-lb-entry__stat trn-text--right"})
	wins = matchInfo[0].text
	matchesPlayed = matchInfo[1].text


	print("rank: " + rank + "   player: " + player + "   country: " + country + "   wins: " + wins + "   matches played: " + matchesPlayed)


	try :
		f.write( rank + "," + player + "," + country + "," + wins.replace("," ,"") + "," + matchesPlayed.replace("," ,"") + "\n")
	except(UnicodeEncodeError):
		f.write( rank + "," + "????" + "," + country + "," + wins.replace("," ,"") + "," + matchesPlayed.replace("," ,"") + "\n")




f.close()