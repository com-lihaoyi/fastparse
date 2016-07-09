class Book2 {
    public enum Genre { NOVEL, SCIFI, HISTORICAL, TEXTBOOK }

    private String title;
    private int pubYear;
    private Genre genre;
    private int copies;

    public String getTitle() {
        return title;
    }

    public int getPubYear() {
        return pubYear;
    }

    public Genre getGenge() {
        return genre;
    }

    public int getCopies() {
        return copies;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setPubYear(int pubYear) {
        this.pubYear = pubYear;
    }

    public void setGenre(Genre genre) {
        this.genre = genre;
    }

    public void setCopies(int copies) {
        this.copies = copies;
    }

    @Override
    public String toString() {
        String copiesString;
        switch (copies) {
            case 0: copiesString = "No copies"; break;
            case 1: copiesString = "Only one copy"; break;
            case 2: copiesString = "Two copies"; break;
            default: copiesString = "A lot of copies!"; break;
        }
        return title + " " + String.valueOf(pubYear) + " " + genre.name() + " " + copiesString;
    }
}

