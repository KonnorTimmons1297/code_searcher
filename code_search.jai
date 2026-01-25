#import "Basic";
#import "Window_Creation";
#import "Input";
#import "Simp";
#import "GetRect_LeftHanded";
#import "String";
#import "File";
#import "File_Utilities";
#import "Thread";
#import "Math";
#import "Hash_Table";
#import "Text_File_Handler";
#import "System";

DEBUG :: false;

window_width  : s32 = 1920;
window_height : s32 = 1080;

theme: Overall_Theme;

TheFont: *Dynamic_Font;
FontSize := 18;
LineHeight := 0.0;
UI_FRAME_THICKNESS := 3.0;

LoadFont :: () {
    TheFont = get_font_at_size("data", "ProggyVector-Regular.ttf", FontSize);
    LineHeight = xx TheFont.character_height * 1.5;
}

scroll_region_value: float;
scroll_region_value_for_file_contents: float;
scroll_region_value_for_search_dirs: float;

ClearQueryResults :: () {
    lock(*QueryResultsMutex);
    array_reset(*QueryResults);
    unlock(*QueryResultsMutex);
}

ExecuteQuery :: (query: string) {
    if query.count == 0 then return;

	ClearQueryResults();

    for file_index: 0..Files.count-1 {
        file: *LoadedFile = *Files[file_index];
		//log("Executing Query for file: %", file.path);
	    if !file.loaded {
	        log("Loading file %", file.path);
	        file.contents, file.loaded = read_entire_file(file.path);
	        if !file.loaded then {
	            log("Failed to load file %", file.path);
	            return;
	        }

	        dup_ptr := file.contents;
	        start_of_line_index := 0;
	        end_of_line_index := 0;
	        for index: 0..file.contents.count-1 {
	        	char := file.contents[index];
	        	if char == #char "\n" {
	        		end_of_line_index := index;
	        		line: Line;
	        		line.index = start_of_line_index;
	        		line.text = file.contents;
	        		line.text.count = index+1;
	        		advance(*line.text, start_of_line_index);
					//line.text = trim_right(line.text);
					//line.text = replace(line.text, "\t", "    ");
					// line.text = trim(line.text);

					// log("line %: %", file.lines.count+1, line.text);

	        		array_add(*file.lines, line);

	        		// "abcd1234\nsjdflaksdjf\n"
	        		//            ^
	        		//       start of line

	        		start_of_line_index = index+1;
	        	}
	        }
	    }
	    if file.loaded {
	    	for line: file.lines {
	    		if contains(line.text, query) {
		    		query_result: QueryResult;
		            query_result.file_index = file_index;
		            query_result.line_index = it_index;
		            lock(*QueryResultsMutex);
		            array_add(*QueryResults, query_result);
		            unlock(*QueryResultsMutex);
	    		}
	    	}
	    }
    }
}

Line :: struct {
	index := -1;
	text: string;
}
LastQuery: string;
LoadedFile :: struct {
    name: string;
    path: string;
    contents: string;
    lines: [..]Line;
    loaded := false;

	path_drawn := false; // Used by UI only
}
Files: [..]LoadedFile;

QueryResult :: struct {
    line_index := -1;
    file_index := -1;
}
QueryResults: [..]QueryResult;

Query: string;
SelectedQueryResult: *QueryResult;
SearchDirectoryInput: string;

QueryResultsMutex: Mutex;
QueryMutex: Mutex;
DirectoriesToSearchMutex: Mutex;
ExecutingQuery := false;

DirectoryState :: struct {
	path: string;
	scanned := false;
	removed := false;
}

VaildFiletypes: [..]string;
DirectoriesToSearch: [..]DirectoryState;

ScanDirectory :: (directory: *DirectoryState) {
	log("Scanning directory: %", directory.path);

	DirectoryScanProgress :: struct {
		files_added := 0;
		files: *[..]LoadedFile;
	}
	progress: DirectoryScanProgress;
	progress.files = *Files;

	visitor :: (info: *File_Visit_Info, progress: *DirectoryScanProgress) {
        is_text_file := false;
        for filetype: VaildFiletypes {
            if ends_with(info.short_name, filetype) {
                is_text_file = true;
                break;
            }
        }
        if !is_text_file then return;
        if begins_with(info.short_name, ".") then return;

        loaded_file: LoadedFile;
        loaded_file.name = copy_string(info.short_name);
        loaded_file.path = copy_string(info.full_name);
        array_add(progress.files, loaded_file);
        progress.files_added += 1;
    }
	visit_files(directory.path, recursive=true, *progress, visitor, visit_directories=true);
	directory.scanned = true;

	log("Scan complete. Added % files", progress.files_added);
}

QueryThreadInitialized := false;
QueryThread: Thread;
QueryThreadProc :: (thread: *Thread) -> s64 {
	while true {
        lock(*QueryMutex);
        query_updated := LastQuery != Query;
        if query_updated then LastQuery = Query;
        unlock(*QueryMutex);

		lock(*DirectoriesToSearchMutex);
			removed_directories: [..]DirectoryState;
			removed_directories.allocator = temp;

			for *directory: DirectoriesToSearch {
				if directory.removed {
					array_add(*removed_directories, directory.*);
					remove directory;
					continue;
				} else if !directory.scanned {
					ScanDirectory(directory);
				}
			}

		unlock(*DirectoriesToSearchMutex);

		if removed_directories.count {
			ClearQueryResults(); // do not want anyone touching the data if/when it's freed.
		}

		for removed_directory: removed_directories {
			log("Removing directory: %", removed_directory.path);

			for *file, file_index: Files {
				if contains(file.path, removed_directory.path) {
					if file.contents then free(file.contents);
					remove file;
				}
			}
		}

        if query_updated {
            log("Query updated: %", LastQuery);
            ExecuteQuery(LastQuery);
        }

        sleep_milliseconds(8);
    }

    return 0;
}

IsInsideRect :: (a: Rect, r: Rect) -> bool {
	top_in_rect := a.y >= r.y && a.y <= (r.y + r.h);
	bottom_in_rect := (a.y+a.h) >= r.y && (a.y+a.h) <= (r.y + r.h);
	return top_in_rect || bottom_in_rect;
}

DrawQuerySelection :: (destination_rect: Rect) {
	scrollable_region_theme := theme.scrollable_region_theme;
	scrollable_region_theme.region_background.shape.rounding_flags = 0;
	scrollable_region, results_display_rect := begin_scrollable_region(destination_rect, *scrollable_region_theme);

    result_item_rect := results_display_rect;
    result_item_rect.h = LineHeight;
	result_item_rect.y -= scroll_region_value;

    lock(*QueryResultsMutex);
    for *QueryResults {
    	file := Files.data + it.file_index;

    	// NOTE: Keeping track of drawn file names in an array,
    	//       we check to see if the file name has been drawn 
    	//       yet. If it hasn't, then we draw the file name!
		if !file.path_drawn {
			if IsInsideRect(result_item_rect, results_display_rect) {
				text := tprint("File: %", file.path);
				label(result_item_rect, text, *theme.label_theme);
			}
    		result_item_rect.y += LineHeight;
			file.path_drawn = true;
		}

		if IsInsideRect(result_item_rect, results_display_rect) {
    		line := file.lines[it.line_index];
			button_text := tprint("L%: %", it.line_index+1, trim(line.text));
			pressed := button(result_item_rect, button_text, *theme.button_theme, identifier = it_index);
			if pressed {
				SelectedQueryResult = it;
				y_value := SelectedQueryResult.line_index * TheFont.character_height;
				scroll_region_value_for_file_contents = xx y_value;
			}
		}
        result_item_rect.y += LineHeight;
    }
    for *QueryResults {
    	file := Files.data + it.file_index;
		file.path_drawn = false;
	}
    unlock(*QueryResultsMutex);

    end_scrollable_region(scrollable_region, 0, result_item_rect.y + result_item_rect.h, *scroll_region_value);
}


AddSearchDirectory :: (path: string) {
	directory: DirectoryState;
	directory.path = path;
	lock(*DirectoriesToSearchMutex);
	array_add(*DirectoriesToSearch, directory);
	unlock(*DirectoriesToSearchMutex);
}

// SettingsSubwindow: Subwindow_Info;
// AddDirectoryInput: string;
// DrawSettingsSubwindow :: (state: *Subwindow_State, r: Rect, data: *void) {
// 	line_height := TheFont.character_height*1.5;
// 	my_rect := r;
// 	my_rect.h = line_height;
// 	{
// 		label(my_rect, "Search Directories", *theme.label_theme);
// 	}
// 	my_rect.y += my_rect.h;

// 	{
// 		add_directory_input_rect := my_rect;
// 		result_action, new_text, state := text_input(add_directory_input_rect, AddDirectoryInput, *theme.text_input_theme);
// 	    if result_action & .TEXT_MODIFIED {
// 	        modify_string(*AddDirectoryInput, state.text);
// 	    } else if result_action & .ENTERED {
// 	    	directory_to_add := copy_string(AddDirectoryInput);
// 	    	AddSearchDirectory(directory_to_add);
// 	    	state.text = "";
// 	    	modify_string(*AddDirectoryInput, "");
// 	    }
// 	}
//     my_rect.y += my_rect.h;

// 	{
// 		search_directory_list_rect := my_rect;
// 		search_directory_list_rect.h = r.h;
// 		x_glyph_width := prepare_text(TheFont, "-") * 3;
// 		scrollable_region_theme := theme.scrollable_region_theme;
// 	    scrollable_state, scrollabe_rect :=
// 	    	begin_scrollable_region(search_directory_list_rect, *scrollable_region_theme);

// 	    search_directory_item_rect := scrollabe_rect;
// 	    search_directory_item_rect.h = line_height;
// 		search_directory_item_rect.y -= scroll_region_value_for_search_dirs;

// 		directory_label_rect := search_directory_item_rect;
// 		directory_label_rect.w -= x_glyph_width;

// 		remove_directory_button_rect := directory_label_rect;
// 		remove_directory_button_rect.x += directory_label_rect.w;
// 		remove_directory_button_rect.w = xx x_glyph_width;

// 		lock(*DirectoriesToSearchMutex);

// 			for *directory: DirectoriesToSearch {
// 				label(search_directory_item_rect, directory.path, *theme.label_theme);
// 				remove_pressed := button(remove_directory_button_rect, "-", *theme.button_theme, identifier = it_index);
// 				if remove_pressed {
// 					directory.removed = true;
// 				}
// 				search_directory_item_rect.y += search_directory_item_rect.h;
// 				remove_directory_button_rect.y += remove_directory_button_rect.h;
// 			}

// 		unlock(*DirectoriesToSearchMutex);

// 	    end_scrollable_region(scrollable_state, 0, search_directory_item_rect.y, *scroll_region_value_for_search_dirs);
// 	}
// }

AfterSearchCriteriaChanged :: () {
	SelectedQueryResult = null;
}

// TODO: Consider moving the searching logic into it's own file

main :: () {

	init(*QueryResultsMutex);
    init(*QueryMutex);
	init(*DirectoriesToSearchMutex);

	exe_filepath := get_path_of_running_executable();
	exe_filepath.count = find_index_from_right(exe_filepath, "/");
	settings_filename := "code_search_settings.txt";
	settings_file_filepath := sprint("%/%", exe_filepath, settings_filename);
	
	if file_exists(settings_file_filepath) {
		tfh: Text_File_Handler;
		start_file(*tfh, "settings", settings_file_filepath);
		if tfh.failed {
			log_error("Failed to load settings");
			return;
		}

		Readmode :: enum {
			None;
			Filetypes;
			Directories;
		}

		mode := Readmode.None;
		line, found := consume_next_line(*tfh);
		while found {
			if line == ":: Filetypes" {
				mode = .Filetypes;
			} else if line == ":: Directories" {
				mode = .Directories;
			} else {
				if mode == {
					case .Filetypes; {
						filetype := copy_string(line);
						if !begins_with(filetype, ".") {
							modify_string(*filetype, sprint(".%", line));
						}
						array_add(*VaildFiletypes, filetype);
						log("Filetype: %", line);
					}
					case .Directories; {
						dir: DirectoryState;
						dir.path = copy_string(line);
						array_add(*DirectoriesToSearch, dir);
						log("Directory: %", dir.path);
					}
				}
			}
			line, found = consume_next_line(*tfh);
		}

		deinit(*tfh);
	} else {
		array_add(*VaildFiletypes, ".jai");
		AddSearchDirectory(copy_string(get_working_directory()));
	}

	initial_search_directory_input_sb: String_Builder;
	init_string_builder(*initial_search_directory_input_sb);
	for dir: DirectoriesToSearch {
		print_to_builder(*initial_search_directory_input_sb, "%;", dir.path);
	}
	for filetype: VaildFiletypes {
		print_to_builder(*initial_search_directory_input_sb, "%;", tprint("*%", filetype));
	}

    Query = copy_string("");
    SearchDirectoryInput = copy_string(builder_to_string(*initial_search_directory_input_sb));

    thread_initialized := thread_init(*QueryThread, QueryThreadProc);
    if !thread_initialized {
        log_error("Failed to initialize query thread!");
        return;
    }
    thread_start(*QueryThread);

    

    window := create_window(window_width, window_height, "Code Searcher");

	set_render_target(window, .LEFT_HANDED);

	set_global_frame_thickness_in_pixels(UI_FRAME_THICKNESS);
	ui_init();

    LoadFont();

    // SettingsSubwindow.rect = get_rect(100, 100, 0.15 * window_width, TheFont.character_height*20.0);
	// SettingsSubwindow.title_text = "Settings";
	// SettingsSubwindow.draw = DrawSettingsSubwindow;
	// SettingsSubwindow.open = false;

    last_loop_elapsed_time: float64 = 0;
    program_is_running := true;
    while program_is_running {
        time_at_beginning_of_loop := current_time_monotonic();

		update_window_events();

		for get_window_resizes() {
			update_window(it.window);

			if it.window == window {
                window_width  = it.width;
                window_height = it.height;

                LoadFont();
                theme.font = TheFont;
            }
		}

		for events_this_frame {
			if it.type == .QUIT {
				program_is_running = false;
			} else if it.type == .KEYBOARD {
				if it.key_code == #char "+" && it.ctrl_pressed {
					FontSize += 2;
					LoadFont();
				} else if it.key_code == #char "-" && it.ctrl_pressed {
					FontSize -= 2;
					LoadFont();
				}
			}
			getrect_handle_event(it);
		}

		clear_render_target(0.25, 0.25, 0.25, 1.0);
    	x, y, width, height := get_dimensions(window, true);
    	ui_per_frame_update(window, width, height, seconds_since_init());

    	base_theme := default_theme_procs[0]();
        theme = base_theme;
        theme.font = TheFont;
        theme.button_theme.font = theme.font;
        theme.button_theme.alignment = .Left;
        theme.label_theme.font = TheFont;
        theme.label_theme.alignment = .Left;
        theme.text_input_theme.font = TheFont;
        theme.scrollable_region_theme.region_background.shape.rounding_flags = 0;
        set_default_theme(theme);

        search_directories_label_rect: Rect;
        search_directories_label_rect.w = xx prepare_text(TheFont, "Env  ");
        search_directories_label_rect.h = LineHeight;

        label(search_directories_label_rect, "Env", *theme.label_theme);

        search_directories_input_rect: Rect;
        search_directories_input_rect.x += search_directories_label_rect.w;
        search_directories_input_rect.w = (window_width - search_directories_label_rect.w);
        search_directories_input_rect.h = LineHeight;

        { // search directories input
            result_action, new_text, state := text_input(search_directories_input_rect, SearchDirectoryInput, *theme.text_input_theme);
            if result_action & .TEXT_MODIFIED {
            	log("SearchDirectoryInput=%", SearchDirectoryInput);
            	modify_string(*SearchDirectoryInput, state.text);
            } else if result_action & .ENTERED {
            	input_directories := split(SearchDirectoryInput, ";",, temp);
            	for user_input_directory: input_directories {
            		if trim(user_input_directory).count == 0 {
            			continue;
            		}

            		if user_input_directory[0] == #char "." {
            			// TODO:
            			// Check if already added to valid list
            			// Add to list if not already added
            			// Tell the system to rescan for those files
            		}

            		directory_exists := file_exists(user_input_directory);
            		if !directory_exists {
            			log_error("Directory '%' is malformed or does not exist", user_input_directory);
            			continue;
            		}

					directory_loaded := false;

            		lock(*DirectoriesToSearchMutex);
            		for tracked_directory: DirectoriesToSearch {
            			if tracked_directory.path == user_input_directory {
            				directory_loaded = true;
            				break;
            			}
            		}
            		unlock(*DirectoriesToSearchMutex);

            		if !directory_loaded {
            			AddSearchDirectory(copy_string(user_input_directory));
            		}
            	}

	        	lock(*DirectoriesToSearchMutex);
				for *search_dir: DirectoriesToSearch {
					if !array_find(input_directories, search_dir.path) {
						search_dir.removed = true;
					}
				}
				unlock(*DirectoriesToSearchMutex);
            }
        } // search directories input

        // settings_button_rect: Rect;
        // settings_button_rect.h = TheFont.character_height*1.5;
        // settings_button_rect.w = prepare_text(TheFont, "Settings")*1.5;
        // settings_button_rect.x = window_width - settings_button_rect.w;

        query_label_rect: Rect;
        query_label_rect.y += LineHeight;
        query_label_rect.h = LineHeight;
        query_label_rect.w = xx prepare_text(TheFont, "Query  ");

        label(query_label_rect, "Query", *theme.label_theme);

        query_input_rect: Rect;
        query_input_rect.x += query_label_rect.w;
        query_input_rect.y += LineHeight;
        query_input_rect.h = LineHeight;
        query_input_rect.w = (window_width - query_label_rect.w);

        { // search text input
            result_action, new_text, state := text_input(query_input_rect, Query, *theme.text_input_theme);
            if result_action & .TEXT_MODIFIED {
                lock(*QueryMutex);
                modify_string(*Query, state.text);
                unlock(*QueryMutex);
                AfterSearchCriteriaChanged();
            }
        } // search text input

        if SelectedQueryResult {
        	selected_file := Files[SelectedQueryResult.file_index];
			selected_line := selected_file.lines[SelectedQueryResult.line_index];

        	slidable_rect: Rect;
            slidable_rect.h = window_height - (2*query_input_rect.h);
            slidable_rect.w = xx window_width;
            slidable_rect.y = query_input_rect.y + query_input_rect.h;

			slidable_theme := *theme.slidable_region_theme;
			slidable_theme.orientation = .VERTICAL;
			slidable_theme.first_background.shape.rounding_flags = 0;
			slidable_theme.second_background.shape.rounding_flags = 0;
            slidable_state, first_rect, second_rect, divider_r := slidable_region(slidable_rect, slidable_theme);

			DrawQuerySelection(first_rect);

            switch_to_second_rect(slidable_state);

			{ // begin selection preview section
	            selection_preview_rect := second_rect;
	            selection_preview_rect.y += 4.0; // this is global_frame_thickness

	            file_path_rect := selection_preview_rect;
				file_path_rect.h = LineHeight;
	            file_path_rect.y += UI_FRAME_THICKNESS;
	            label(file_path_rect, selected_file.path, *theme.label_theme);
				selection_preview_rect.y += file_path_rect.h;
				selection_preview_rect.h -= file_path_rect.h + 2*UI_FRAME_THICKNESS;

				{ // begin file contents region
					scrollable_region_theme := theme.scrollable_region_theme;
					scrollable_region_theme.region_background.shape.rounding_flags = 0;
					scrollable_region, file_contents_scrollable_rect := begin_scrollable_region(selection_preview_rect, *scrollable_region_theme);

					file_contents_line_rect := file_contents_scrollable_rect;
					file_contents_line_rect.h = xx TheFont.character_height;
					line_spacing := 0.0;
					file_contents_line_rect.h += line_spacing;
					file_contents_line_rect.y += 10.0;
					file_contents_line_rect.y -= scroll_region_value_for_file_contents;
					for line_index: 0..selected_file.lines.count-1 {
						file_contents_line_rect.x = file_contents_scrollable_rect.x;
						line := selected_file.lines[line_index];

						if line_index == SelectedQueryResult.line_index {
							draw_procs.set_shader_for_color();
							p0 := Vector2.{ file_contents_line_rect.x, file_contents_line_rect.y };
							p1 := p0 + .{ file_contents_line_rect.w, 0 };
							p2 := p0 + .{ file_contents_line_rect.w, file_contents_line_rect.h };
							p3 := p0 + .{ 0, file_contents_line_rect.h };
							draw_procs.immediate_quad(
								p0, p1, p2, p3,
								color = theme.background_color_bright);
							draw_procs.immediate_flush();
						}

						// TODO: Do this when the file is loaded
						current_line := trim_right(line.text);
						current_line = replace(current_line, "\t", "    ",, temp);

						// current_line := line.text;//trim_right(line.text);
						//normalized_line_text = replace(normalized_line_text, "\t", "    ");

						// if contains(current_line, #char "\t") {
						// 	space_width := prepare_text(TheFont, " ");
						// 	how_much_to_advance_line := 0;

						// 	for line_char, line_index: current_line {
						// 		if line_char != #char "\t" then break;
						// 		how_much_to_advance_line += 1;
						// 		file_contents_line_rect.x += space_width * 4;
						// 	}

						// 	advance(*current_line, how_much_to_advance_line);
						// }

						// if current_line.count > 0 && current_line[current_line.count-1] == #char "\n" {
						// 	current_line.count -= 1;
						// }

						if IsInsideRect(file_contents_line_rect, file_contents_scrollable_rect) {
							label(file_contents_line_rect, current_line, *theme.label_theme);
						}
						file_contents_line_rect.y += file_contents_line_rect.h;
					}

					end_scrollable_region(scrollable_region, 0, file_contents_line_rect.y, *scroll_region_value_for_file_contents);
				} // end file contents region
			} // end selection preview section

			finish(slidable_state);
        } else {
            r: Rect;
            r.h = window_height - (query_input_rect.y + query_input_rect.h);
            r.w = xx window_width;
            r.y = query_input_rect.y + query_input_rect.h;

            DrawQuerySelection(r);
        }

        // if SettingsSubwindow.open {
        // 	add_subwindow(*SettingsSubwindow, *theme.subwindow_theme);
        // }

        draw_popups();

        swap_buffers(window);

		reset_temporary_storage();

        sleep_milliseconds(10);

        last_loop_elapsed_time = to_float64_seconds(current_time_monotonic() - time_at_beginning_of_loop);
	}

	{
		settings_sb: String_Builder;
		init_string_builder(*settings_sb);
		print_to_builder(*settings_sb, "[1]\n\n");

		print_to_builder(*settings_sb, ":: Filetypes\n");
		for filetype: VaildFiletypes {
			print_to_builder(*settings_sb, "%\n", filetype);
		}

		print_to_builder(*settings_sb, "\n:: Directories\n");
		for directory: DirectoriesToSearch {
			print_to_builder(*settings_sb, "%\n", directory.path);
		}

		if !write_entire_file(settings_file_filepath, builder_to_string(*settings_sb)) {
			log("Failed to update settings");
		}
	}
}

modify_string :: (ps: *string, new_value: string) {
    free(ps.*);
    ps.* = copy_string(new_value);
}
